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
    
    mymean <- mean(as.numeric(as.character(data$reactionTime)));
    
    means <- c(means,mymean);
    
    #extracrt participant id
    m <- gregexpr('[0-9]+',mfile);
    p_id <- regmatches(mfile,m);
    
    
    participantNumbers <- c(participantNumbers,as.character(p_id));
  }
  
  meanParticipantReaction<- data.frame(participantNumbers,means, stringsAsFactors = FALSE);
  return(meanParticipantReaction);
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

#male, physical, ar and no task files
malePhysicalAR <- list.files(path = malePath,
                               recursive = TRUE,
                               pattern = "*pyn.csv");

meansMalePhysicalAR <- calculateMeansOfObs(malePath,malePhysicalAR);

#male, physical, ar and task files
malePhysicalARTask <- list.files(path = malePath,
                             recursive = TRUE,
                             pattern = "*pyy.csv");

meansMalePhysicalARTask <- calculateMeansOfObs(malePath,malePhysicalARTask);

#male, virtual, no ar and no task files
maleVirtualNoAR <- list.files(path = malePath,
                               recursive = TRUE,
                               pattern = "*vnn.csv");

meansMaleVirtualNoAR <- calculateMeansOfObs(malePath,maleVirtualNoAR);

#male, virtual, ar and no task files
maleVirtualAR <- list.files(path = malePath,
                             recursive = TRUE,
                             pattern = "*vyn.csv");

meansMaleVirtualAR <- calculateMeansOfObs(malePath,maleVirtualAR);

#male, virtual, ar and task files
maleVirtualARTask <- list.files(path = malePath,
                                 recursive = TRUE,
                                 pattern = "*vyy.csv");
meansMaleVirtualARTask <- calculateMeansOfObs(malePath,maleVirtualARTask);

#female, physical, no ar and no task files
femalePhysicalNoAR <- list.files(path = femalePath,
                               recursive = TRUE,
                               pattern = "*pnn.csv");
meansFemalePhysicalNoAR <- calculateMeansOfObs(femalePath,femalePhysicalNoAR);

#female, physical, ar and no task files
femalePhysicalAR <- list.files(path = femalePath,
                             recursive = TRUE,
                             pattern = "*pyn.csv");
meansFemalePhysicalAR <- calculateMeansOfObs(femalePath,femalePhysicalAR);

#female, physical, ar and task files
femalePhysicalARTask <- list.files(path = femalePath,
                                 recursive = TRUE,
                                 pattern = "*pyy.csv");

meansFemalePhysicalARTask <- calculateMeansOfObs(femalePath,femalePhysicalARTask);

#female, virtual, no ar and no task files
femaleVirtualNoAR <- list.files(path = femalePath,
                              recursive = TRUE,
                              pattern = "*vnn.csv");
meansFemaleVirtualNoAR <- calculateMeansOfObs(femalePath,femaleVirtualNoAR);

#female, virtual, ar and no task files
femaleVirtualAR <- list.files(path = femalePath,
                            recursive = TRUE,
                            pattern = "*vyn.csv");
meansFemaleVirtualAR <- calculateMeansOfObs(femalePath,femaleVirtualAR);

#female, virtual, ar and task files
femaleVirtualARTask <- list.files(path = femalePath,
                                recursive = TRUE,
                                pattern = "*vyy.csv");

meansFemaleVirtualARTask <- calculateMeansOfObs(femalePath,femaleVirtualARTask);

