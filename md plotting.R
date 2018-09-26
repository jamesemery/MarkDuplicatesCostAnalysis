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

plotTimeVsCost <- function(df, controls) {
  mod <- filter(df, time > 0)
  mod <- na.omit(mod)
  mod$Cores <- as.factor(mod$cores)
  p <- ggplot(data=mod, aes(x=time, y=cost, shape=Preemptible, ymin=0, xmin=0), color=Cores) 
  p <- p + geom_point(aes(color=as.factor(cores))) 
  p <- addControlToCostPlot(p, controls[2], controls[1], "Picard MD + SS")
  p <- p + theme_bw()
  p <- p + ylab("Cost ($)")
  p <- p + xlab("Time (Hours)")
  p <- p + labs(title="Cost vs Time of MarkDuplicatesSpark", subtitle="Run with various parameters")
  p <- p + labs(color = "Number of Cores")
  p <- p + labs(shape = "Preemption")
  print(p)
}


addControlToCostPlot <- function(p, x, y, label) {
  p <- p + geom_point(x=x ,y=y, color="purple", show.legend=FALSE)
  p <- p + geom_vline(xintercept=x, alpha = .5)
  p <- p + geom_hline(yintercept=y, alpha = .5)
  p <- p + geom_label(x = x - 2, y = y +.2, label=label)
  return(p)
}

plotTimeVsCost(summed2,controlstotal2)


reformatData <- function(df) {
  mod <- df %>% mutate(persisient = str_replace(persisient, "p", "Preemptible")) %>% mutate(persisient = str_replace(persisient, "x", "non-Preemptible"))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, " attached to Preemptible VMs", ""))  %>% mutate(sku_description = str_replace(sku_description, "Preemptible ", ""))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, " from Americas to Americas", ""))
  mod <- mod %>% mutate(sku_description = str_replace(sku_description, " running in Americas", ""))
  mod$Preemptible <- mod$persisient
  mod$persisient <- NULL
  
  mod$cores <- as.integer(mod$cores)
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

plotCoresvsCostBreakdown <-function(df) {
  dfp <- df %>% filter(  spark_loader=="disq" && memory==15 && disk_space==375 ); dfp
  
  dfp <- dfp %>% filter( cost > 0)
  
  grouped <- group_by(dfp, sku_description,  Preemptible, cores) %>% summarise_at(vars(cost), sum, na.rm = TRUE)
  
  ggplot(data=grouped, aes(x=as.numeric(cores), y=cost, fill=sku_description)) + geom_area() + facet_grid(. ~Preemptible)
}

plotMemoryVsCostOverSparkLoader <- function(df) {
  dfp <- df %>% filter( memory!="208" && execution_type=="newtool" && spark_loader!="nioinput"  && disk_space==375 && Preemptible!="Preemptible"); dfp
  
  grouped <- group_by(dfp, memory,  Preemptible, spark_loader, cores) %>% summarise_at(vars(cost), sum, na.rm = TRUE)
  
  ggplot(data=grouped, aes(x=as.numeric(memory), y=cost, color=cores)) + geom_line() + geom_point() + facet_grid(. ~spark_loader)
}


plotCoresvsCostBreakdownPicardControl <-function(dfp) {
  dfp <- shrunkbam2 %>% filter( execution_type!="newtool" && execution_type!="noop" ); dfp
  dfp
  dfp <- dfp %>% filter( cost > 0)
  
  tmpa <- dfp #%>% filter(!grepl("Sorted",execution_type, fixed=TRUE)); 
  tmpa <- separate(data = tmpa, col = execution_type, into = c("execution_type", "Premptible"), sep = "-")
  tmpa <- tmpa %>% mutate(Premptible = str_replace(Premptible, "p", "Premptible"))
  tmpa$Premptible <- ifelse(is.na(tmpa$Premptible), 'non-Premptible', tmpa$Premptible); tmpa
  tmpa <- tmpa %>% mutate(execution_type = str_replace(execution_type, "markduplicatespersistent", "MD-persistent disk"))
  tmpa <- tmpa %>% mutate(execution_type = str_replace(execution_type, "markduplicatesssd", "MD-SSD"))
  tmpa <- tmpa %>% mutate(execution_type = str_replace(execution_type, "picardmarkedsortedssd", "Sort-SSD"))
  tmpa <- tmpa %>% mutate(execution_type = str_replace(execution_type, "sortsampersistent", "Sort-persistent disk"))
  
  grouped <- group_by(tmpa, sku_description, execution_type, Premptible) %>% summarise_at(vars(cost), sum, na.rm = TRUE)
  grouped
  ggplot(data=grouped, aes(x=execution_type, y=cost, fill=sku_description)) + geom_bar(stat="identity") + facet_grid(. ~Premptible)
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
summed3 <- shrunkbam2 %>% summarise_at(vars(cost:time), sum, na.rm = TRUE)
summed3

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

plotMemoryVsCostOverSparkLoader(shrunkbam1)

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
plotTimeVsCost(summed,controlstotalm)
plotTimeVsCost(summed3,controlstotal3)
plotTimeVsCost(summed2,controlstotal2)
plotTimeVsCost(summed1,controlstotal2) #very bad
plotTimeVsCostMemoryFlavor(summed,controlstotalm)

