setwd("/Users/bsyiem/Documents/Rscripts");

#used to easily plot qqplots
library("car");

#for two way Anova
library(tidyverse);
library(ggpubr);
library(rstatix);

#returns the means of each sample concatenated in a list
## returns a data fram with
# p_id
# means of RTs
# means of RTs for peripheral leds
# means of RTs for central led.
calculateMeansOfObs <- function(mpath,mfiles){
  meanRT <- c();
  centralMeanRT <- c();
  peripheralMeanRT <- c();
  
  participantNumbers <- c();
  for(mfile in mfiles){
    mfilePath <- paste(mpath,"/",mfile,sep = "");
    
    data <- read.csv(mfilePath, header = FALSE);
    colnames(data) <- c("ledNumber", "reactionTime", "reactionType");
    
    #cleaning all rows with NA or blanks
    data <- data[complete.cases(data),];
    data <- data[!(data$ledNumber == "" | data$reactionTime == "" | data$reactionType == "")
                 &(data$reactionType == 'TP'),];
    
    #data is the main data frame with RTs for all leds
    
    
    #led number 4 is the central led and is the only physical led that is not visible through peripheral vision

    #dataPeripheral is the data frame with RTs for peripheral leds
    dataPeripheral <- data[(data$ledNumber != "4"),];
    
    #dataCentral is the data frame with RTs for the central led
    dataCentral <- data[(data$ledNumber == "4"),];
    
    #trimming about 5 values or 0.2 of the data from each end
    totalMean <- mean(as.numeric(as.character(data$reactionTime)),trim = 0.2);
    
    #less data for this so we won't trim
    #might trim later 
    #TODO
    meanPeripheral <- mean(as.numeric(as.character(dataPeripheral$reactionTime)),trim = 0.2);
    meanCenter <- mean(as.numeric(as.character(dataCentral$reactionTime)),trim = 0.2);
    
    #round the mean as our precision is only for microseconds
    meanRT <- c(meanRT,round(totalMean));
    peripheralMeanRT <- c(peripheralMeanRT,round(meanPeripheral));
    centralMeanRT <- c(centralMeanRT,round(meanCenter));
    
    
    #extracrt participant id
    m <- gregexpr('[0-9]+',mfile);
    p_id <- regmatches(mfile,m);
    
    
    participantNumbers <- c(participantNumbers,as.character(p_id));
  }
  
  meanParticipantReaction<- data.frame(pid = as.factor(participantNumbers), meanRT, peripheralMeanRT, centralMeanRT, stringsAsFactors = FALSE);
  return(meanParticipantReaction);
}

addGender <- function(dataFrame,gender){
  dataFrame <- cbind(dataFrame,gender = rep(gender,nrow(dataFrame)));
  return(dataFrame);
}

addEvent <- function(dataFrame,event){
  dataFrame <- cbind(dataFrame,event = rep(event,nrow(dataFrame)));
  return(dataFrame);
}

addScenario <- function(dataFrame,scenario){
  dataFrame <- cbind(dataFrame,scenario = rep(scenario,nrow(dataFrame)));
  return(dataFrame);
}

#all file paths
malePath <- "../PhD_(MAIN)/Study\ 2/Study\ 2\ Data/Study/Raw/Male";
femalePath <- "../PhD_(MAIN)/Study\ 2/Study\ 2\ Data/Study/Raw/Female";


########################################################################
#Males
########################################################################

#male, physical, no ar and no task files
malePhysicalNoAR <- list.files(path = malePath,
                               recursive = TRUE,
                               pattern = "*pnn.csv");

meansMalePhysicalNoAR <- calculateMeansOfObs(malePath,malePhysicalNoAR);
meansMalePhysicalNoAR <- addGender(meansMalePhysicalNoAR,"M");
meansMalePhysicalNoAR <- addEvent(meansMalePhysicalNoAR,"P")
meansMalePhysicalNoAR <- addScenario(meansMalePhysicalNoAR,"NC");

#male, physical, ar and no task files
malePhysicalAR <- list.files(path = malePath,
                             recursive = TRUE,
                             pattern = "*pyn.csv");

meansMalePhysicalAR <- calculateMeansOfObs(malePath,malePhysicalAR);
meansMalePhysicalAR <- addGender(meansMalePhysicalAR,"M");
meansMalePhysicalAR <- addEvent(meansMalePhysicalAR,"P");
meansMalePhysicalAR <- addScenario(meansMalePhysicalAR,"VC");


#male, physical, ar and task files
malePhysicalARTask <- list.files(path = malePath,
                                 recursive = TRUE,
                                 pattern = "*pyy.csv");

meansMalePhysicalARTask <- calculateMeansOfObs(malePath,malePhysicalARTask);
meansMalePhysicalARTask <- addGender(meansMalePhysicalARTask,"M");
meansMalePhysicalARTask <- addEvent(meansMalePhysicalARTask,"P");
meansMalePhysicalARTask <- addScenario(meansMalePhysicalARTask,"VCT")

#male, virtual, no ar and no task files
maleVirtualNoAR <- list.files(path = malePath,
                              recursive = TRUE,
                              pattern = "*vnn.csv");

meansMaleVirtualNoAR <- calculateMeansOfObs(malePath,maleVirtualNoAR);
meansMaleVirtualNoAR <- addGender(meansMaleVirtualNoAR,"M");
meansMaleVirtualNoAR <- addEvent(meansMaleVirtualNoAR,"V");
meansMaleVirtualNoAR <- addScenario(meansMaleVirtualNoAR,"NC");

#male, virtual, ar and no task files
maleVirtualAR <- list.files(path = malePath,
                            recursive = TRUE,
                            pattern = "*vyn.csv");

meansMaleVirtualAR <- calculateMeansOfObs(malePath,maleVirtualAR);
meansMaleVirtualAR <- addGender(meansMaleVirtualAR,"M");
meansMaleVirtualAR <- addEvent(meansMaleVirtualAR,"V");
meansMaleVirtualAR <- addScenario(meansMaleVirtualAR,"VC");

#male, virtual, ar and task files
maleVirtualARTask <- list.files(path = malePath,
                                recursive = TRUE,
                                pattern = "*vyy.csv");

meansMaleVirtualARTask <- calculateMeansOfObs(malePath,maleVirtualARTask);
meansMaleVirtualARTask <- addGender(meansMaleVirtualARTask,"M");
meansMaleVirtualARTask <- addEvent(meansMaleVirtualARTask,"V");
meansMaleVirtualARTask <- addScenario(meansMaleVirtualARTask,"VCT");

########################################################################
#Females
########################################################################

#female, physical, no ar and no task files
femalePhysicalNoAR <- list.files(path = femalePath,
                                 recursive = TRUE,
                                 pattern = "*pnn.csv");

meansFemalePhysicalNoAR <- calculateMeansOfObs(femalePath,femalePhysicalNoAR);
meansFemalePhysicalNoAR <- addGender(meansFemalePhysicalNoAR,"F");
meansFemalePhysicalNoAR <- addEvent(meansFemalePhysicalNoAR,"P");
meansFemalePhysicalNoAR <- addScenario(meansFemalePhysicalNoAR,"NC");

#female, physical, ar and no task files
femalePhysicalAR <- list.files(path = femalePath,
                               recursive = TRUE,
                               pattern = "*pyn.csv");

meansFemalePhysicalAR <- calculateMeansOfObs(femalePath,femalePhysicalAR);
meansFemalePhysicalAR <- addGender(meansFemalePhysicalAR, "F");
meansFemalePhysicalAR <- addEvent(meansFemalePhysicalAR, "P");
meansFemalePhysicalAR <- addScenario(meansFemalePhysicalAR, "VC");

#female, physical, ar and task files
femalePhysicalARTask <- list.files(path = femalePath,
                                   recursive = TRUE,
                                   pattern = "*pyy.csv");

meansFemalePhysicalARTask <- calculateMeansOfObs(femalePath,femalePhysicalARTask);
meansFemalePhysicalARTask <- addGender(meansFemalePhysicalARTask, "F");
meansFemalePhysicalARTask <- addEvent(meansFemalePhysicalARTask, "P");
meansFemalePhysicalARTask <- addScenario(meansFemalePhysicalARTask,"VCT");

#female, virtual, no ar and no task files
femaleVirtualNoAR <- list.files(path = femalePath,
                                recursive = TRUE,
                                pattern = "*vnn.csv");

meansFemaleVirtualNoAR <- calculateMeansOfObs(femalePath,femaleVirtualNoAR);
meansFemaleVirtualNoAR <- addGender(meansFemaleVirtualNoAR,"F");
meansFemaleVirtualNoAR <- addEvent(meansFemaleVirtualNoAR,"V");
meansFemaleVirtualNoAR <- addScenario(meansFemaleVirtualNoAR,"NC");

#female, virtual, ar and no task files
femaleVirtualAR <- list.files(path = femalePath,
                              recursive = TRUE,
                              pattern = "*vyn.csv");

meansFemaleVirtualAR <- calculateMeansOfObs(femalePath,femaleVirtualAR);
meansFemaleVirtualAR <- addGender(meansFemaleVirtualAR,"F"); 
meansFemaleVirtualAR <- addEvent(meansFemaleVirtualAR,"V"); 
meansFemaleVirtualAR <- addScenario(meansFemaleVirtualAR,"VC");

#female, virtual, ar and task files
femaleVirtualARTask <- list.files(path = femalePath,
                                  recursive = TRUE,
                                  pattern = "*vyy.csv");

meansFemaleVirtualARTask <- calculateMeansOfObs(femalePath,femaleVirtualARTask);
meansFemaleVirtualARTask <- addGender(meansFemaleVirtualARTask,"F");
meansFemaleVirtualARTask <- addEvent(meansFemaleVirtualARTask,"V");
meansFemaleVirtualARTask <- addScenario(meansFemaleVirtualARTask,"VCT");

########################################################################
#ALL
########################################################################

#means for all genders
meansPhysicalNoAR <- rbind(meansMalePhysicalNoAR,meansFemalePhysicalNoAR);
meansPhysicalAR <- rbind(meansMalePhysicalAR,meansFemalePhysicalAR);
meansPhysicalARTask <- rbind(meansMalePhysicalARTask,meansFemalePhysicalARTask);
meansVirtualNoAR <- rbind(meansMaleVirtualNoAR,meansFemaleVirtualNoAR);
meansVirtualAR <- rbind(meansMaleVirtualAR,meansFemaleVirtualAR);
meansVirtualARTask <- rbind(meansMaleVirtualARTask,meansFemaleVirtualARTask);


#########################################################################
# FINAL DATA FRAME
#########################################################################

myData <- rbind(
  meansPhysicalNoAR,
  meansPhysicalAR,
  meansPhysicalARTask,
  meansVirtualNoAR,
  meansVirtualAR,
  meansVirtualARTask);

#########################################
# summary statistics
#########################################
myData %>% group_by(scenario,event) %>% get_summary_stats(meanRT,type = "mean_sd");

#########################################
# visualizations
#########################################
bxp <- ggboxplot(
  myData, x = "event", y = "meanRT",
  color = "scenario", palette = "jco"
);
bxp;

#########################################
# Assumptions
#########################################
#outlier
myData %>% group_by(scenario,event) %>% identify_outliers(meanRT);
#shapiro test
myData %>% group_by(scenario,event) %>% shapiro_test(meanRT);
#qqplots
ggqqplot(myData,"meanRT", ggtheme = theme_bw()) + 
  facet_grid(event ~ scenario, labeller = "label_both");

######################################################
# REPEATED MEASURE ANOVA
######################################################

rt.aov <- anova_test(data = myData,dv = meanRT, wid = pid, within = c(scenario,event));
get_anova_table(rt.aov);

lm(meanRT~pid+event:scenario,data = myData);
