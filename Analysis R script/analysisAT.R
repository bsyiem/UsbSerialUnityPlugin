setwd("/Users/bsyiem/Documents/Rscripts");

#used to easily plot qqplots
library("car");

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
  
  meanParticipantReaction<- data.frame(participantNumbers, meanRT, peripheralMeanRT, centralMeanRT, stringsAsFactors = FALSE);
  return(meanParticipantReaction);
}

addGender <- function(dataFrame,gender){
  dataFrame <- cbind(dataFrame,gender = rep(gender,nrow(dataFrame)));
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
meansMalePhysicalNoAR <- addScenario(meansMalePhysicalNoAR,"PNAR");

#male, physical, ar and no task files
malePhysicalAR <- list.files(path = malePath,
                             recursive = TRUE,
                             pattern = "*pyn.csv");

meansMalePhysicalAR <- calculateMeansOfObs(malePath,malePhysicalAR);
meansMalePhysicalAR <- addGender(meansMalePhysicalAR,"M");
meansMalePhysicalAR <- addScenario(meansMalePhysicalAR,"PAR");


#male, physical, ar and task files
malePhysicalARTask <- list.files(path = malePath,
                                 recursive = TRUE,
                                 pattern = "*pyy.csv");

meansMalePhysicalARTask <- calculateMeansOfObs(malePath,malePhysicalARTask);
meansMalePhysicalARTask <- addGender(meansMalePhysicalARTask,"M");
meansMalePhysicalARTask <- addScenario(meansMalePhysicalARTask,"PART")

#male, virtual, no ar and no task files
maleVirtualNoAR <- list.files(path = malePath,
                              recursive = TRUE,
                              pattern = "*vnn.csv");

meansMaleVirtualNoAR <- calculateMeansOfObs(malePath,maleVirtualNoAR);
meansMaleVirtualNoAR <- addGender(meansMaleVirtualNoAR,"M");
meansMaleVirtualNoAR <- addScenario(meansMaleVirtualNoAR,"VNAR");

#male, virtual, ar and no task files
maleVirtualAR <- list.files(path = malePath,
                            recursive = TRUE,
                            pattern = "*vyn.csv");

meansMaleVirtualAR <- calculateMeansOfObs(malePath,maleVirtualAR);
meansMaleVirtualAR <- addGender(meansMaleVirtualAR,"M");
meansMaleVirtualAR <- addScenario(meansMaleVirtualAR,"VAR");

#male, virtual, ar and task files
maleVirtualARTask <- list.files(path = malePath,
                                recursive = TRUE,
                                pattern = "*vyy.csv");

meansMaleVirtualARTask <- calculateMeansOfObs(malePath,maleVirtualARTask);
meansMaleVirtualARTask <- addGender(meansMaleVirtualARTask,"M");
meansMaleVirtualARTask <- addScenario(meansMaleVirtualARTask,"VART");

########################################################################
#Females
########################################################################

#female, physical, no ar and no task files
femalePhysicalNoAR <- list.files(path = femalePath,
                                 recursive = TRUE,
                                 pattern = "*pnn.csv");

meansFemalePhysicalNoAR <- calculateMeansOfObs(femalePath,femalePhysicalNoAR);
meansFemalePhysicalNoAR <- addGender(meansFemalePhysicalNoAR,"F");
meansFemalePhysicalNoAR <- addScenario(meansFemalePhysicalNoAR,"PNAR");

#female, physical, ar and no task files
femalePhysicalAR <- list.files(path = femalePath,
                               recursive = TRUE,
                               pattern = "*pyn.csv");

meansFemalePhysicalAR <- calculateMeansOfObs(femalePath,femalePhysicalAR);
meansFemalePhysicalAR <- addGender(meansFemalePhysicalAR, "F");
meansFemalePhysicalAR <- addScenario(meansFemalePhysicalAR, "PAR");

#female, physical, ar and task files
femalePhysicalARTask <- list.files(path = femalePath,
                                   recursive = TRUE,
                                   pattern = "*pyy.csv");

meansFemalePhysicalARTask <- calculateMeansOfObs(femalePath,femalePhysicalARTask);
meansFemalePhysicalARTask <- addGender(meansFemalePhysicalARTask, "F");
meansFemalePhysicalARTask <- addScenario(meansFemalePhysicalARTask,"PART");

#female, virtual, no ar and no task files
femaleVirtualNoAR <- list.files(path = femalePath,
                                recursive = TRUE,
                                pattern = "*vnn.csv");

meansFemaleVirtualNoAR <- calculateMeansOfObs(femalePath,femaleVirtualNoAR);
meansFemaleVirtualNoAR <- addGender(meansFemaleVirtualNoAR,"F");
meansFemaleVirtualNoAR <- addScenario(meansFemaleVirtualNoAR,"VNAR");

#female, virtual, ar and no task files
femaleVirtualAR <- list.files(path = femalePath,
                              recursive = TRUE,
                              pattern = "*vyn.csv");

meansFemaleVirtualAR <- calculateMeansOfObs(femalePath,femaleVirtualAR);
meansFemaleVirtualAR <- addGender(meansFemaleVirtualAR,"F"); 
meansFemaleVirtualAR <- addScenario(meansFemaleVirtualAR,"VAR");

#female, virtual, ar and task files
femaleVirtualARTask <- list.files(path = femalePath,
                                  recursive = TRUE,
                                  pattern = "*vyy.csv");

meansFemaleVirtualARTask <- calculateMeansOfObs(femalePath,femaleVirtualARTask);
meansFemaleVirtualARTask <- addGender(meansFemaleVirtualARTask,"F");
meansFemaleVirtualARTask <- addScenario(meansFemaleVirtualARTask,"VART");

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
#check for normality 
#########################################

#males
qqPlot(meansMalePhysicalNoAR$means);
qqPlot(meansMalePhysicalAR$means);
qqPlot(meansMalePhysicalARTask$means);
qqPlot(meansMaleVirtualNoAR$means);
qqPlot(meansMaleVirtualAR$means);
qqPlot(meansMaleVirtualARTask$means);

#females
qqPlot(meansFemalePhysicalNoAR$means);
qqPlot(meansFemalePhysicalAR$means);
qqPlot(meansFemalePhysicalARTask$means);
qqPlot(meansFemaleVirtualNoAR$means);
qqPlot(meansFemaleVirtualAR$means);
qqPlot(meansFemaleVirtualARTask$means);

#all
qqPlot(meansPhysicalNoAR$means);
qqPlot(meansPhysicalAR$means);
qqPlot(meansPhysicalARTask$means);
qqPlot(meansVirtualNoAR$means);
qqPlot(meansVirtualAR$means);
qqPlot(meansVirtualARTask$means);

######################################################
# FOR REPEATED MEASURE ANOVA
######################################################

