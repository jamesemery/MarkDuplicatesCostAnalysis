library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(dplyr)
library(stringr)
library(tidyr)
library(gridExtra)

loadTable <- function(input){
  newMD <- read.csv(input, stringsAsFactors = FALSE)
  return(newMD)
}

collectPairedMDControl <- function(df, task1, task2) {
  tmp <- df %>% filter(execution_type==task1| execution_type == task2) 
  tmp <- tmp %>% summarise_at(vars(cost:time), sum, na.rm = TRUE)
  return(c(sum(tmp$cost),sum(tmp$time)))
}

plotTimeVsCost <- function(df) {
  mod <- filter(df, time > 0)
  mod <- na.omit(mod)
  mod$Cores <- as.factor(mod$cores)
  
  p <- ggplot(data=mod, aes(x=time, y=cost, shape=Preemptible, ymin=0, xmin=0), color=Cores) 
  p <- p + geom_point(aes(color=as.factor(cores))) 
  p <- p + theme_bw()
  p <- p + ylab("Cost ($)")
  p <- p + xlab("Time (Hours)")
  p <- p + labs(title="Cost vs Time of MarkDuplicatesSpark", subtitle="Run with varied parameters")
  p <- p + labs(color = "Number of Cores")
  p <- p + labs(shape = "Preemption")
  
  pdf <- filter(df, execution_type != "newtool")
  pdf <- filter(pdf, !grepl("ssd", execution_type) && execution_type != "noop")
  pdf$cores <- 1
  pdf$Preemptible <- pdf$Preemptible <- ifelse(grepl("-p", pdf$execution_type), 'Preemptible', 'non-Preemptible')
  
  mdPre <- filter(pdf, Preemptible == "Preemptible")
  p <- addControlToCostPlot(p, sum(mdPre$time), sum(mdPre$cost), NULL )
  mdNon <- filter(pdf, Preemptible == "non-Preemptible")
  p <- addControlToCostPlot(p, sum(mdNon$time), sum(mdNon$cost), "Picard MD + SS")
  
  
  p <- p + geom_point(data=pdf)
  
  print(p)
}




addControlToCostPlot <- function(p, x, y, label) {
  p <- p + geom_point(x=x ,y=y, color="purple", show.legend=FALSE)
  p <- p + geom_vline(xintercept=x, alpha = .8, linetype="dotted")
  p <- p + geom_hline(yintercept=y, alpha = .8, linetype="dotted")
  if( length(label) > 0 ){
    p <- p + geom_label(x = x - 2, y = y +.2, label=label)
  }
  return(p)
}


reformatData <- function(df) {
  mod <- df %>% mutate(persisient = str_replace(persisient, "p", "Preemptible")) %>% mutate(persisient = str_replace(persisient, "x", "non-Preemptible"))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, " attached to Preemptible VMs", ""))  %>% mutate(sku_description = str_replace(sku_description, "Preemptible ", ""))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, " from Americas to Americas", ""))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, " running in Americas", ""))
  mod$Preemptible <- mod$persisient
  mod$persisient <- NULL
  
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, "Custom instance Core", "Core"))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, "Custom instance Ram", "Ram"))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, "Network Internet Egress", "Egress"))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, "SSD backed Local Storage", "SSD"))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, "Storage PD Capacity", "HDD"))
  
  mod$cores <- as.integer(mod$cores)
  mod$memory <- as.numeric(mod$memory)
  return(mod)
}

splitBamRuntimeDataToFormat <- function(df, runtimeToIgnore) {
  tmp <- df %>% filter(!((!grepl("newtool",labels_value,fixed=TRUE))&grepl(runtimeToIgnore,labels_value,fixed=TRUE)))
  tmp <- separate(data = tmp, col = labels_value, into = c("labels_value", "execution_type","taskname"), sep = ",")
  tmpa <- tmp %>% filter(!grepl("newtool",execution_type, fixed=TRUE)); 
  tmpa <- separate(data = tmpa, col = execution_type, into = c("execution_type", "spark_loader","cores","memory","disk_space","persisient"), sep = "\\?")
  
  tmpb <- tmp %>% filter(grepl("newtool",execution_type, fixed=TRUE)); 
  tmpb <- separate(data = tmpb, col = execution_type, into = c("execution_type", "spark_loader","cores","memory","disk_space","persisient"), sep = "-+")
  tmpb <- tmpb %>% mutate(memory = str_replace(memory, "gb", ""))
  return(reformatData(rbind(tmpa,tmpb)))
}

plotMemoryVsCostOverSparkLoader <- function(df) {

  df$memory <- mapply(adjustMemory, df$memory, df$cores)
  df$cores <- as.factor(df$cores)
  dfp <- df %>% filter( execution_type=="newtool" && spark_loader!="nioinput"  && disk_space==375 && spark_loader=="disq"); dfp
  
  grouped <- group_by(dfp, memory,  Preemptible, spark_loader, cores) %>% summarise_at(vars(cost, time), sum, na.rm = TRUE)
  grouped
  grouped <- filter(grouped, time > 0.0)
  p <- ggplot(data=grouped, aes(x=as.numeric(memory), y=cost, color=cores))
  p <- p + geom_line() + geom_point() 
  p <- p + facet_grid(. ~Preemptible)
  p <- p + labs(title = "Cost vs Memory for MarkDuplicatesSpark",
                subtitle="Split by Preemption and Grouped by Number of Cores",
                color = "Number of Cores")
  p <- p + ylab("Cost ($)")
  p <- p + xlab("Memory (Gb)")
  p <- p + theme_bw()
  print(p)
}


plotCoresvsCostBreakdown <-function(df) {
  dfp <- df %>% filter(  spark_loader=="disq" && memory==15 && disk_space==375 ); dfp
  
  dfp <- dfp %>% filter( cost > 0)
  dfp <- filter(dfp, sku_description != "Egress")
  grouped <- group_by(dfp, sku_description,  Preemptible, cores) %>% summarise_at(vars(cost), sum, na.rm = TRUE)
  
  p <- ggplot(data=grouped, aes(x=as.numeric(cores), y=cost, fill=sku_description)) 
  p <- p + geom_area()
  p <- p + facet_grid(. ~Preemptible)
  p <- p + labs(title="MarkDuplicatesSpark Cost Breakdown by Cores",
                subtitle="Subdivided by Preemptible", fill="Cost Category")
  p <- p + xlab("Number of Cores") 
  p <- p + ylab("Cost ($)")
  p <- p + theme_bw()
          
  print(p)
}

plotCoresvsCostBreakdownPicardControl <-function(dfp) {
  dfp <- shrunkbam2 %>% filter( execution_type!="newtool" && execution_type!="noop" ); dfp
  dfp
  dfp <- dfp %>% filter( cost > 0)
  
  tmpa <- dfp #%>% filter(!grepl("Sorted",execution_type, fixed=TRUE)); 
  tmpa <- separate(data = tmpa, col = execution_type, into = c("execution_type", "Preemptible"), sep = "-")
  tmpa <- tmpa %>% mutate(mutatedPreemptible = str_replace(Preemptible, "p", "Preemptible"))
  tmpa$mutatedPreemptible <- ifelse(is.na(tmpa$mutatedPreemptible), 'non-Preemptible', tmpa$mutatedPreemptible); tmpa
  tmpa <- filter( tmpa, grepl("sort",execution_type))
  tmpa <- tmpa %>% mutate(execution_type = str_replace(execution_type, "markduplicatespersistent", "MD-persistent disk"))
  tmpa <- tmpa %>% mutate(execution_type = str_replace(execution_type, "markduplicatesssd", "MD-SSD"))
  tmpa <- tmpa %>% mutate(execution_type = str_replace(execution_type, "picardmarkedsortedssd", "SSD"))
  tmpa <- tmpa %>% mutate(execution_type = str_replace(execution_type, "sortsampersistent", "HDD"))
  
  grouped <- group_by(tmpa, sku_description, execution_type, mutatedPreemptible) %>% summarise_at(vars(cost), sum, na.rm = TRUE)
  grouped
  p <- ggplot(data=grouped, aes(x=execution_type, y=cost, fill=sku_description)) 
  p <- p + geom_bar(stat="identity") + facet_grid(. ~mutatedPreemptible)
  p <- p + xlab("Disk Type")
  p <- p + ylab("Cost (%)")
  p <- p + labs(title="Picard SortSam cost breakdown on SSD and HDD",
                subtitle("Subdivided by Preemption"),
                fill="Cost Category")
  p <- p + theme_bw()
  print(p)
}


plotTimeVsCostMemoryFlavor <- function(df, controls) {
  p <- ggplot(data=df, aes(x=time, y=cost, shape=Preemptible))
  p <- p + geom_point(aes(color=memory))
  p <- p + xlim(c(0.5,12)) + coord_cartesian(x=c(0,12))
  p <- p + continuous_scale_colour_brewer()
  p <- p + geom_point(aes(x=controls[2],y=controls[1]))
  p <- p + geom_vline(xintercept =controls[2])
  p <- p + geom_hline(yintercept =controls[1])
  p <- p + theme_bw()
  print(p)
}
######################################## Inputting Data ########################################

trialMedium <- "big_trial.manipulated.csv"
trialbam1 <- "marktimingbam1.csv"
trialbam2 <- "results-bam2.csv"
trialbam3 <- "marktimingbam3.csv"
tblm <- reformatData(loadTable(trialMedium))
tbl1 <- loadTable(trialbam1)
tbl2 <- loadTable(trialbam2)
tbl3 <- loadTable(trialbam3)


bam1 <- splitBamRuntimeDataToFormat(tbl1, "2c3a5bed-98ea-4f73-9f46-3f3000bfc284")
bam2 <- splitBamRuntimeDataToFormat(tbl2,"0bfdbf14-d7d9-4d2f-b48b-56832fccde14")
bam3 <- splitBamRuntimeDataToFormat(tbl3, "c22c3981-efa0-4488-b003-5564f3fa5483")

## Making the separeated data
shrunk <- tblm %>% group_by(execution_type, spark_loader, cores, memory, disk_space, Preemptible) 
summed <- shrunk %>% summarise_at(vars(cost:time), sum, na.rm = TRUE); summed

shrunkbam1 <- bam1 %>% group_by(execution_type, spark_loader, cores, memory, disk_space, Preemptible)
summed1 <- shrunkbam1 %>% summarise_at(vars(cost:time), sum, na.rm = TRUE)
summed1
shrunkbam2 <- bam2 %>% group_by(execution_type, spark_loader, cores, memory, disk_space, Preemptible)
summed2 <- shrunkbam2 %>% summarise_at(vars(cost:time), sum, na.rm = TRUE)
summed2
shrunkbam3 <- bam3 %>% group_by(execution_type, spark_loader, cores, memory, disk_space, Preemptible)
summed3 <- shrunkbam3 %>% summarise_at(vars(cost:time), sum, na.rm = TRUE)
summed3

#change memory to be the minimum memory given the number of cores if less was requested
adjustMemory <- function(memory, cores){
  return(max(memory, 0.9 * cores))
}



######################################## Plotting ########################################
## Plotting Cores vs. Cost Breakdown
plotCoresvsCostBreakdown(shrunkbam1)
plotCoresvsCostBreakdownPicardControl(shrunkbam1)
plotCoresvsCostBreakdown(shrunkbam2)
plotCoresvsCostBreakdownPicardControl(shrunkbam2)
plotCoresvsCostBreakdown(shrunkbam3)
plotCoresvsCostBreakdownPicardControl(shrunkbam3)
## Making the separeated data
## Plotting memory vs cost

plotMemoryVsCostOverSparkLoader(shrunkbam2)

## plotting various things 
summed1 %>% filter(!str_detect(execution_type, "newtool")) %>% summarise_at(vars(cost:time), sum, na.rm = TRUE); 
summed1 %>% filter(!str_detect(execution_type, "newtool"))

controlData <- shrunk %>% filter(!str_detect(execution_type, "newtool")) %>% summarise_at(vars(cost:time), sum, na.rm = TRUE); 
summed2 %>% filter(execution_type!="newtool")
controlstotalm <- collectPairedMDControl(summed,"markduplicatesssd","picardmarkedsortedssd")
controlstotal2preemptable <- collectPairedMDControl(summed2,"markduplicatespersistent-p","sortsampersistent-p")
controlstotal2 <- collectPairedMDControl(summed2,"markduplicatespersistent","sortsampersistent")
controlstotal3 <- collectPairedMDControl(summed3,"markduplicatespersistent","sortsampersistent")
controlstotal2
plotTimeVsCost(summed)
plotTimeVsCost(summed3)
plotTimeVsCost(summed2)
plotTimeVsCost(summed1) #very bad
plotTimeVsCostMemoryFlavor(summed,controlstotalm)

