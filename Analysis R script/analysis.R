setwd("/Users/bsyiem/Documents/Rscripts");


#returns the means of each sample concatenated in a list
calculateMeansOfObs <- function(mpath,mfiles){
  means <- c();
  participantNumbers <- c();
  for(mfile in mfiles){
    mfilePath <- paste(mpath,"/",mfile,sep = "");
    
    data <- read.csv(mfilePath, header = FALSE);
    colnames(data) <- c("ledNumber", "reactionTime", "reactionType");
    
    #cleaning all rows with NA or blanks
    data <- data[complete.cases(data),];
    data <- data[!(data$ledNumber == "" | data$reactionTime == "" | data$reactionType == "")
                 &(data$reactionType == 'TP'),];
    
    #trimming about 5 values or 0.2 of the data from each end
    mymean <- mean(as.numeric(as.character(data$reactionTime)),trim = 0.2);
    #mymean <- mean(as.numeric(as.character(data$reactionTime)),trim = 0);
    
    #round the mean as our precision is only for microseconds
    means <- c(means,round(mymean));
    
    #extracrt participant id
    m <- gregexpr('[0-9]+',mfile);
    p_id <- regmatches(mfile,m);
    
    
    participantNumbers <- c(participantNumbers,as.character(p_id));
  }
  
  meanParticipantReaction<- data.frame(participantNumbers,means, stringsAsFactors = FALSE);
  return(meanParticipantReaction);
}

##gets the means of participant reaction time for the central led
calculateMeansOfNonPeripheral <- function(mpath,mfiles){
  means <- c();
  participantNumbers <- c();
  for(mfile in mfiles){
    mfilePath <- paste(mpath,"/",mfile,sep = "");
    
    data <- read.csv(mfilePath, header = FALSE);
    colnames(data) <- c("ledNumber", "reactionTime", "reactionType");
    
    #cleaning all rows with NA or blanks
    data <- data[complete.cases(data),];
    data <- data[!(data$ledNumber == "" | data$reactionTime == "" | data$reactionType == "")
                 &(data$reactionType == 'TP'),];
    
    #get only values for when ledNumber = 4 or the centered led which is not seen in periphery
    data <- data[(data$ledNumber == "4"),];
    
    meanCenter <- mean(as.numeric(as.character(data$reactionTime)),trim = 0);
    
    #round the mean as our precision is only for microseconds
    means <- c(means,round(meanCenter));
    
    #extracrt participant id
    m <- gregexpr('[0-9]+',mfile);
    p_id <- regmatches(mfile,m);
    
    
    participantNumbers <- c(participantNumbers,as.character(p_id));
  }
  
  meanCenterReaction<- data.frame(participantNumbers,means, stringsAsFactors = FALSE);
  return(meanCenterReaction);
}

##gets the means of participant reaction time for the non central leds
calculateMeansOfPeripheral <- function(mpath,mfiles){
  means <- c();
  participantNumbers <- c();
  for(mfile in mfiles){
    mfilePath <- paste(mpath,"/",mfile,sep = "");
    
    data <- read.csv(mfilePath, header = FALSE);
    colnames(data) <- c("ledNumber", "reactionTime", "reactionType");
    
    #cleaning all rows with NA or blanks
    data <- data[complete.cases(data),];
    data <- data[!(data$ledNumber == "" | data$reactionTime == "" | data$reactionType == "")
                 &(data$reactionType == 'TP'),];
    
    #get only values for when ledNumber != 4 or the centered led which is not seen in periphery
    data <- data[(data$ledNumber != "4"),];
    
    meanPeripheral <- mean(as.numeric(as.character(data$reactionTime)),trim = 0);
    
    #round the mean as our precision is only for microseconds
    means <- c(means,round(meanPeripheral));
    
    #extracrt participant id
    m <- gregexpr('[0-9]+',mfile);
    p_id <- regmatches(mfile,m);
    
    
    participantNumbers <- c(participantNumbers,as.character(p_id));
  }
  
  meanPeripheralReaction<- data.frame(participantNumbers,means, stringsAsFactors = FALSE);
  return(meanPeripheralReaction);
}

#all file paths
rawPath <- "../PhD_(MAIN)/Study\ 2/Study\ 2\ Data/Study/Raw";
malePath <- "../PhD_(MAIN)/Study\ 2/Study\ 2\ Data/Study/Raw/Male";
femalePath <- "../PhD_(MAIN)/Study\ 2/Study\ 2\ Data/Study/Raw/Female";

#reads all raw csv files 
dataFiles <- list.files(path = rawPath,
                        recursive = TRUE,
                        pattern = "*.csv");

#male, physical, no ar and no task files
malePhysicalNoAR <- list.files(path = malePath,
                           recursive = TRUE,
                           pattern = "*pnn.csv");

meansMalePhysicalNoAR <- calculateMeansOfObs(malePath,malePhysicalNoAR);
meansMalePhysicalNoARCenter <- calculateMeansOfNonPeripheral(malePath,malePhysicalNoAR);
meansMalePhysicalNoARPeripheral <- calculateMeansOfPeripheral(malePath,malePhysicalNoAR);

#male, physical, ar and no task files
malePhysicalAR <- list.files(path = malePath,
                               recursive = TRUE,
                               pattern = "*pyn.csv");

meansMalePhysicalAR <- calculateMeansOfObs(malePath,malePhysicalAR);
meansMalePhysicalARCenter <- calculateMeansOfNonPeripheral(malePath,malePhysicalAR);
meansMalePhysicalARPeripheral <- calculateMeansOfPeripheral(malePath,malePhysicalAR);

#male, physical, ar and task files
malePhysicalARTask <- list.files(path = malePath,
                             recursive = TRUE,
                             pattern = "*pyy.csv");

meansMalePhysicalARTask <- calculateMeansOfObs(malePath,malePhysicalARTask);
meansMalePhysicalARTaskCenter <- calculateMeansOfNonPeripheral(malePath,malePhysicalARTask);
meansMalePhysicalARTaskPeripheral <- calculateMeansOfPeripheral(malePath,malePhysicalARTask);

#male, virtual, no ar and no task files
maleVirtualNoAR <- list.files(path = malePath,
                               recursive = TRUE,
                               pattern = "*vnn.csv");

meansMaleVirtualNoAR <- calculateMeansOfObs(malePath,maleVirtualNoAR);
meansMaleVirtualNoARCenter <- calculateMeansOfNonPeripheral(malePath,maleVirtualNoAR);
meansMaleVirtualNoARPeripheral <- calculateMeansOfPeripheral(malePath,maleVirtualNoAR);

#male, virtual, ar and no task files
maleVirtualAR <- list.files(path = malePath,
                             recursive = TRUE,
                             pattern = "*vyn.csv");

meansMaleVirtualAR <- calculateMeansOfObs(malePath,maleVirtualAR);
meansMaleVirtualARCenter <- calculateMeansOfNonPeripheral(malePath,maleVirtualAR);
meansMaleVirtualARPeripheral <- calculateMeansOfPeripheral(malePath,maleVirtualAR);

#male, virtual, ar and task files
maleVirtualARTask <- list.files(path = malePath,
                                 recursive = TRUE,
                                 pattern = "*vyy.csv");

meansMaleVirtualARTask <- calculateMeansOfObs(malePath,maleVirtualARTask);
meansMaleVirtualARTaskCenter <- calculateMeansOfNonPeripheral(malePath,maleVirtualARTask);
meansMaleVirtualARTaskPeripheral <- calculateMeansOfPeripheral(malePath,maleVirtualARTask);

#female, physical, no ar and no task files
femalePhysicalNoAR <- list.files(path = femalePath,
                               recursive = TRUE,
                               pattern = "*pnn.csv");

meansFemalePhysicalNoAR <- calculateMeansOfObs(femalePath,femalePhysicalNoAR);
meansFemalePhysicalNoARCenter <- calculateMeansOfNonPeripheral(femalePath,femalePhysicalNoAR);
meansFemalePhysicalNoARPeripheral <- calculateMeansOfPeripheral(femalePath,femalePhysicalNoAR);

#female, physical, ar and no task files
femalePhysicalAR <- list.files(path = femalePath,
                             recursive = TRUE,
                             pattern = "*pyn.csv");

meansFemalePhysicalAR <- calculateMeansOfObs(femalePath,femalePhysicalAR);
meansFemalePhysicalARCenter <- calculateMeansOfNonPeripheral(femalePath,femalePhysicalAR);
meansFemalePhysicalARPeripheral <- calculateMeansOfPeripheral(femalePath,femalePhysicalAR);

#female, physical, ar and task files
femalePhysicalARTask <- list.files(path = femalePath,
                                 recursive = TRUE,
                                 pattern = "*pyy.csv");

meansFemalePhysicalARTask <- calculateMeansOfObs(femalePath,femalePhysicalARTask);
meansFemalePhysicalARTaskCenter <- calculateMeansOfNonPeripheral(femalePath,femalePhysicalARTask);
meansFemalePhysicalARTaskPeripheral <- calculateMeansOfPeripheral(femalePath,femalePhysicalARTask);

#female, virtual, no ar and no task files
femaleVirtualNoAR <- list.files(path = femalePath,
                              recursive = TRUE,
                              pattern = "*vnn.csv");

meansFemaleVirtualNoAR <- calculateMeansOfObs(femalePath,femaleVirtualNoAR);
meansFemaleVirtualNoARCenter <- calculateMeansOfNonPeripheral(femalePath,femaleVirtualNoAR);
meansFemaleVirtualNoARPeripheral <- calculateMeansOfPeripheral(femalePath,femaleVirtualNoAR);

#female, virtual, ar and no task files
femaleVirtualAR <- list.files(path = femalePath,
                            recursive = TRUE,
                            pattern = "*vyn.csv");

meansFemaleVirtualAR <- calculateMeansOfObs(femalePath,femaleVirtualAR);
meansFemaleVirtualARCenter <- calculateMeansOfNonPeripheral(femalePath,femaleVirtualAR);
meansFemaleVirtualARPeripheral <- calculateMeansOfPeripheral(femalePath,femaleVirtualAR);

#female, virtual, ar and task files
femaleVirtualARTask <- list.files(path = femalePath,
                                recursive = TRUE,
                                pattern = "*vyy.csv");

meansFemaleVirtualARTask <- calculateMeansOfObs(femalePath,femaleVirtualARTask);
meansFemaleVirtualARTaskCenter <- calculateMeansOfNonPeripheral(femalePath,femaleVirtualARTask);
meansFemaleVirtualARTaskPeripheral <- calculateMeansOfPeripheral(femalePath,femaleVirtualARTask);

####################################################
##example plots
####################################################


hist(meansMalePhysicalNoAR$means);

#males
plot(density(meansMalePhysicalNoAR$means));
plot(density(meansMalePhysicalAR$means));
plot(density(meansMalePhysicalARTask$means));
plot(density(meansMaleVirtualNoAR$means));
plot(density(meansMaleVirtualAR$means));
plot(density(meansMaleVirtualARTask$means));

#females
plot(density(meansFemalePhysicalNoAR$means));
plot(density(meansFemalePhysicalAR$means));
plot(density(meansFemalePhysicalARTask$means));
plot(density(meansFemaleVirtualNoAR$means));
plot(density(meansFemaleVirtualAR$means));
plot(density(meansFemaleVirtualARTask$means));


###################################################################
#INITIAL ANALYSIS
###################################################################

#males
mean(meansMaleVirtualARTask$means);
mean(meansMaleVirtualAR$means);
mean(meansMaleVirtualNoAR$means);
t.test(meansMaleVirtualNoAR$means,meansMaleVirtualAR$means,paired = TRUE, alternative = "two.sided");
t.test(meansMaleVirtualAR$means,meansMaleVirtualARTask$means,paired = TRUE, alternative = "two.sided");


mean(meansMalePhysicalARTask$means);
mean(meansMalePhysicalAR$means);
mean(meansMalePhysicalNoAR$means);
t.test(meansMalePhysicalNoAR$means,meansMalePhysicalAR$means,paired = TRUE, alternative = "two.sided");
t.test(meansMalePhysicalAR$means,meansMalePhysicalARTask$means,paired = TRUE, alternative = "two.sided");

#because of peripheral vision, all the following are significant
# but physical had less reaction time as opposed to virtual which is contrary to literature
t.test(meansMalePhysicalNoAR$means,meansMaleVirtualNoAR$means,paired = TRUE, alternative = "two.sided");
t.test(meansMalePhysicalAR$means,meansMaleVirtualAR$means,paired = TRUE, alternative = "two.sided");
t.test(meansMalePhysicalARTask$means,meansMaleVirtualARTask$means,paired = TRUE, alternative = "two.sided");


#females
mean(meansFemaleVirtualARTask$means);
mean(meansFemaleVirtualAR$means);
mean(meansFemaleVirtualNoAR$means);
t.test(meansFemaleVirtualNoAR$means,meansFemaleVirtualAR$means,paired = TRUE, alternative = "two.sided");
t.test(meansFemaleVirtualAR$means,meansFemaleVirtualARTask$means,paired = TRUE, alternative = "two.sided");


mean(meansFemalePhysicalARTask$means);
mean(meansFemalePhysicalAR$means);
mean(meansFemalePhysicalNoAR$means);
t.test(meansFemalePhysicalNoAR$means,meansFemalePhysicalAR$means,paired = TRUE, alternative = "two.sided");
t.test(meansFemalePhysicalAR$means,meansFemalePhysicalARTask$means,paired = TRUE, alternative = "two.sided");

#because of peripheral vision, all the following are significant
# but physical had less reaction time as opposed to virtual which is contrary to literature
t.test(meansFemalePhysicalNoAR$means,meansFemaleVirtualNoAR$means,paired = TRUE, alternative = "two.sided");
t.test(meansFemalePhysicalAR$means,meansFemaleVirtualAR$means,paired = TRUE, alternative = "two.sided");
t.test(meansFemalePhysicalARTask$means,meansFemaleVirtualARTask$means,paired = TRUE, alternative = "two.sided");

