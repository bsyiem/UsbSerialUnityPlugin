setwd("/Users/bsyiem/Documents/Rscripts");

#used to easily plot qqplots and Anova
library("car");
library("ez");

#for two way Anova
library(tidyverse);
library(ggpubr);
library(rstatix);

#Multilevel Linear Model
library(lme4);
library(lmerTest);

#data formatting
library(dplyr);
library(tidyr);
library(data.table);

library(plyr);

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


##
# M - Male
# F - Female
addGender <- function(dataFrame,gender){
  dataFrame <- cbind(dataFrame,GENDER = rep(gender,nrow(dataFrame)));
  return(dataFrame);
}

##
# P - Physical Event
# V - Virtual Event
addEvent <- function(dataFrame,EVENT){
  dataFrame <- cbind(dataFrame,EVENT = rep(EVENT,nrow(dataFrame)));
  return(dataFrame);
}

##
# NC - No Content
# VC - Virtual Content
# VCT - Virtual Content + Task
addContent <- function(dataFrame,CONTENT){
  dataFrame <- cbind(dataFrame,CONTENT = rep(CONTENT,nrow(dataFrame)));
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
meansMalePhysicalNoAR <- addContent(meansMalePhysicalNoAR,"NC");

#male, physical, ar and no task files
malePhysicalAR <- list.files(path = malePath,
                             recursive = TRUE,
                             pattern = "*pyn.csv");

meansMalePhysicalAR <- calculateMeansOfObs(malePath,malePhysicalAR);
meansMalePhysicalAR <- addGender(meansMalePhysicalAR,"M");
meansMalePhysicalAR <- addEvent(meansMalePhysicalAR,"P");
meansMalePhysicalAR <- addContent(meansMalePhysicalAR,"VC");


#male, physical, ar and task files
malePhysicalARTask <- list.files(path = malePath,
                                 recursive = TRUE,
                                 pattern = "*pyy.csv");

meansMalePhysicalARTask <- calculateMeansOfObs(malePath,malePhysicalARTask);
meansMalePhysicalARTask <- addGender(meansMalePhysicalARTask,"M");
meansMalePhysicalARTask <- addEvent(meansMalePhysicalARTask,"P");
meansMalePhysicalARTask <- addContent(meansMalePhysicalARTask,"VCT")

#male, virtual, no ar and no task files
maleVirtualNoAR <- list.files(path = malePath,
                              recursive = TRUE,
                              pattern = "*vnn.csv");

meansMaleVirtualNoAR <- calculateMeansOfObs(malePath,maleVirtualNoAR);
meansMaleVirtualNoAR <- addGender(meansMaleVirtualNoAR,"M");
meansMaleVirtualNoAR <- addEvent(meansMaleVirtualNoAR,"V");
meansMaleVirtualNoAR <- addContent(meansMaleVirtualNoAR,"NC");

#male, virtual, ar and no task files
maleVirtualAR <- list.files(path = malePath,
                            recursive = TRUE,
                            pattern = "*vyn.csv");

meansMaleVirtualAR <- calculateMeansOfObs(malePath,maleVirtualAR);
meansMaleVirtualAR <- addGender(meansMaleVirtualAR,"M");
meansMaleVirtualAR <- addEvent(meansMaleVirtualAR,"V");
meansMaleVirtualAR <- addContent(meansMaleVirtualAR,"VC");

#male, virtual, ar and task files
maleVirtualARTask <- list.files(path = malePath,
                                recursive = TRUE,
                                pattern = "*vyy.csv");

meansMaleVirtualARTask <- calculateMeansOfObs(malePath,maleVirtualARTask);
meansMaleVirtualARTask <- addGender(meansMaleVirtualARTask,"M");
meansMaleVirtualARTask <- addEvent(meansMaleVirtualARTask,"V");
meansMaleVirtualARTask <- addContent(meansMaleVirtualARTask,"VCT");

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
meansFemalePhysicalNoAR <- addContent(meansFemalePhysicalNoAR,"NC");

#female, physical, ar and no task files
femalePhysicalAR <- list.files(path = femalePath,
                               recursive = TRUE,
                               pattern = "*pyn.csv");

meansFemalePhysicalAR <- calculateMeansOfObs(femalePath,femalePhysicalAR);
meansFemalePhysicalAR <- addGender(meansFemalePhysicalAR, "F");
meansFemalePhysicalAR <- addEvent(meansFemalePhysicalAR, "P");
meansFemalePhysicalAR <- addContent(meansFemalePhysicalAR, "VC");

#female, physical, ar and task files
femalePhysicalARTask <- list.files(path = femalePath,
                                   recursive = TRUE,
                                   pattern = "*pyy.csv");

meansFemalePhysicalARTask <- calculateMeansOfObs(femalePath,femalePhysicalARTask);
meansFemalePhysicalARTask <- addGender(meansFemalePhysicalARTask, "F");
meansFemalePhysicalARTask <- addEvent(meansFemalePhysicalARTask, "P");
meansFemalePhysicalARTask <- addContent(meansFemalePhysicalARTask,"VCT");

#female, virtual, no ar and no task files
femaleVirtualNoAR <- list.files(path = femalePath,
                                recursive = TRUE,
                                pattern = "*vnn.csv");

meansFemaleVirtualNoAR <- calculateMeansOfObs(femalePath,femaleVirtualNoAR);
meansFemaleVirtualNoAR <- addGender(meansFemaleVirtualNoAR,"F");
meansFemaleVirtualNoAR <- addEvent(meansFemaleVirtualNoAR,"V");
meansFemaleVirtualNoAR <- addContent(meansFemaleVirtualNoAR,"NC");

#female, virtual, ar and no task files
femaleVirtualAR <- list.files(path = femalePath,
                              recursive = TRUE,
                              pattern = "*vyn.csv");

meansFemaleVirtualAR <- calculateMeansOfObs(femalePath,femaleVirtualAR);
meansFemaleVirtualAR <- addGender(meansFemaleVirtualAR,"F"); 
meansFemaleVirtualAR <- addEvent(meansFemaleVirtualAR,"V"); 
meansFemaleVirtualAR <- addContent(meansFemaleVirtualAR,"VC");

#female, virtual, ar and task files
femaleVirtualARTask <- list.files(path = femalePath,
                                  recursive = TRUE,
                                  pattern = "*vyy.csv");

meansFemaleVirtualARTask <- calculateMeansOfObs(femalePath,femaleVirtualARTask);
meansFemaleVirtualARTask <- addGender(meansFemaleVirtualARTask,"F");
meansFemaleVirtualARTask <- addEvent(meansFemaleVirtualARTask,"V");
meansFemaleVirtualARTask <- addContent(meansFemaleVirtualARTask,"VCT");

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


##
# changing data use for plotting 
# spelled out conditions
# milliseconds instead of microseconds
myData[,2:4] <- myData[,2:4]/1000 ;

##spelling out acronyms
revalue(myData$GENDER, c("M" = "Male")) -> myData$GENDER;
revalue(myData$GENDER, c("F" = "Female")) -> myData$GENDER;

revalue(myData$EVENT, c("P" = "Physical")) -> myData$EVENT;
revalue(myData$EVENT, c("V" = "Virtual")) -> myData$EVENT;

revalue(myData$CONTENT, c("NC" = "None")) -> myData$CONTENT;
revalue(myData$CONTENT, c("VC" = "Virtual")) -> myData$CONTENT;
revalue(myData$CONTENT, c("VCT" = "Virtual with Task")) -> myData$CONTENT;


#########################################
# summary statistics
#########################################
myData %>% group_by(CONTENT,EVENT) %>% get_summary_stats(meanRT,type = "mean_sd");

#########################################
# visualizations
#########################################

bxp <- ggboxplot(
  myData, x = "CONTENT", y = "meanRT",
  ylab = "RT (milliseconds)",
  color = "EVENT", palette = "jco"
);
bxp;

bxp2 <- ggboxplot(
  myData, x = "EVENT", y = "meanRT",
  ylab = "RT (milliseconds)",
  color = "CONTENT", palette = "jco"
);
bxp2;

#########################################
# Assumptions
#########################################
#outlier
myData %>% group_by(CONTENT,EVENT) %>% identify_outliers(meanRT);
#shapiro test
myData %>% group_by(CONTENT,EVENT) %>% shapiro_test(meanRT);

## since the shapiro-wilk test for VCT CONTENT for both EVENTs are significant
## we also test using the ks test
#kolmogorov-smirnov test
ks.test(
  myData[((myData$EVENT == "Physical") & (myData$CONTENT == "Virtual with Task")),]$meanRT,
  "pnorm",
  mean = mean(myData[((myData$EVENT == "Physical") & (myData$CONTENT == "Virtual with Task")),]$meanRT),
  sd = sd(myData[((myData$EVENT == "Physical") & (myData$CONTENT == "Virtual with Task")),]$meanRT)
);

ks.test(
  myData[((myData$EVENT == "Virtual") & (myData$CONTENT == "Virtual with Task")),]$meanRT,
  "pnorm",
  mean = mean(myData[((myData$EVENT == "Virtual") & (myData$CONTENT == "Virtual with Task")),]$meanRT),
  sd = sd(myData[((myData$EVENT == "Virtual") & (myData$CONTENT == "Virtual with Task")),]$meanRT)
);

#qqplots
ggqqplot(myData,"meanRT", ggtheme = theme_bw()) + 
  facet_grid(EVENT ~ CONTENT, labeller = "label_both");

######################################################
# REPEATED MEASURE ANOVA (Yatani)

# note I am using more than one test for the same calculations
# just to be sure I am getting the correct values
######################################################

#test homogeneity
leveneTest(myData$meanRT~myData$EVENT*myData$CONTENT);

## no repeated measure
options(contrasts = c("contr.sum","contr.poly"));
Anova(lm(myData$meanRT~myData$EVENT*myData$CONTENT),type = "III");

options(contrasts = c("contr.sum","contr.poly"));
rt.ez <- ezANOVA(data = myData,dv = .(meanRT), wid = .(pid), within = .(EVENT,CONTENT), type = 3);
rt.ez;

#this is the same as the above
rt.aov <- aov(meanRT ~ CONTENT*EVENT + Error(pid/(CONTENT*EVENT)), myData);
summary(rt.aov);

######################################################
# Post Hoc Analysis
######################################################


#----------------------------------------------------#
# effect of EVENT on RT (within EVENT)
#----------------------------------------------------#

#using aov
rt.aov.oneway.CONTENTNC <- aov(meanRT ~ EVENT +Error(pid/EVENT),myData[(myData$CONTENT == "None"),]);
summary(rt.aov.oneway.CONTENTNC);
rt.aov.oneway.CONTENTVC <- aov(meanRT ~ EVENT +Error(pid/EVENT),myData[(myData$CONTENT == "Virtual"),]);
summary(rt.aov.oneway.CONTENTVC);
rt.aov.oneway.CONTENTVCT <- aov(meanRT ~ EVENT +Error(pid/EVENT),myData[(myData$CONTENT == "Virtual with Task"),]);
summary(rt.aov.oneway.CONTENTVCT);

#using ez package
ezANOVA(data = myData[(myData$CONTENT == "None"),], dv = .(meanRT), wid = .(pid), within = .(EVENT), type = 3);
ezANOVA(data = myData[(myData$CONTENT == "Virtual"),], dv = .(meanRT), wid = .(pid), within = .(EVENT), type = 3);
ezANOVA(data = myData[(myData$CONTENT == "Virtual with Task"),], dv = .(meanRT), wid = .(pid), within = .(EVENT), type = 3);

#since EVENT only has 1 degree of freedom, a paiwise t test works
pairwise.t.test(
  myData[(myData$CONTENT == "None"),]$meanRT,
  myData[(myData$CONTENT == "None"),]$EVENT,
  paired = TRUE, 
  p.adjust.method = "bonf");
pairwise.t.test(
  myData[(myData$CONTENT == "Virtual"),]$meanRT,
  myData[(myData$CONTENT == "Virtual"),]$EVENT,
  paired = TRUE, 
  p.adjust.method = "bonf");
pairwise.t.test(
  myData[(myData$CONTENT == "Virtual with Task"),]$meanRT,
  myData[(myData$CONTENT == "Virtual with Task"),]$EVENT,
  paired = TRUE, 
  p.adjust.method = "bonf");

#using rstatix
pwc <- myData %>% group_by(CONTENT) %>% pairwise_t_test(meanRT ~ EVENT, paired = TRUE, p.adjust.method = "bonferroni");
pwc;

#plotting PWC
pwc <- pwc %>% add_xy_position(x = "CONTENT");
bxp + 
  font("legend.title", size = 14, face = "bold") +
  font("xlab", size = 14, face = "bold") +
  font("ylab", size = 14, face = "bold") +
  color_palette("Dark2") +
  bgcolor("#F5F5F5") +
  grids(linetype = "dashed", color = "white") +
  stat_pvalue_manual(pwc,label = "p.adj",tip.length = 0,hide.ns = TRUE) +
  labs(
    #    subtitle = "hello",
    caption = get_pwc_label(pwc)
  );
#----------------------------------------------------#
# effect of CONTENT on RT (within CONTENT)
#----------------------------------------------------#

#using aov
rt.aov.oneway.EVENTP<- aov(meanRT ~ CONTENT +Error(pid/CONTENT),myData[(myData$EVENT == "Physical"),]);
summary(rt.aov.oneway.EVENTP);
rt.aov.oneway.EVENTV <- aov(meanRT ~ CONTENT +Error(pid/CONTENT),myData[(myData$EVENT == "Virtual"),]);
summary(rt.aov.oneway.EVENTV);

#using ez
ezANOVA(data = myData[(myData$EVENT == "Physical"),], dv = .(meanRT), wid = .(pid), within = .(CONTENT), type = 3);
ezANOVA(data = myData[(myData$EVENT == "Virtual"),], dv = .(meanRT), wid = .(pid), within = .(CONTENT), type = 3);

# Both are significant so we will move on to pairwise t tests

#using pairwiset.tests
pairwise.t.test(
  myData[(myData$EVENT == "Physical"),]$meanRT,
  myData[(myData$EVENT == "Physical"),]$CONTENT,
  paired =TRUE,
  p.adjust.method = "bonf"
);

pairwise.t.test(
  myData[(myData$EVENT == "Virtual"),]$meanRT,
  myData[(myData$EVENT == "Virtual"),]$CONTENT,
  paired =TRUE,
  p.adjust.method = "bonf"
);

#using rstatix
pwc2 <- myData %>% group_by(EVENT) %>% pairwise_t_test(meanRT ~ CONTENT, paired = TRUE, p.adjust.method = "bonferroni");
pwc2;

# does not work -> does not adjust with bonferroni
#compare_means(meanRT ~ CONTENT,paired = TRUE,method = "t.test", group.by = "EVENT", data = myData, p.adjust.method = "bonferroni");

#plotting PWC2
pwc2 <- pwc2 %>% add_xy_position(x = "EVENT");
bxp2 +
  font("legend.title", size = 14, face = "bold") +
  font("xlab", size = 14, face = "bold") +
  font("ylab", size = 14, face = "bold") +
  color_palette("Dark2") +
  #  color_palette(c("#FF5A00","#55AAAA", "#0000FF")) +
  bgcolor("#F5F5F5") +
  grids(linetype = "dashed", color = "#ECECEC") +
  stat_pvalue_manual(pwc2,tip.length = 0.01,label = "p.adj", hide.ns = TRUE) +
  labs(
    #    subtitle = "hello",
    caption = get_pwc_label(pwc2)
  );

######################################################
# TEST
# REPEATED MEASURE ANOVA (rstatix)
# THIS IS NOT WORKING
######################################################

rt.aov <- anova_test(data = myData,dv = meanRT, wid = pid, within = c(CONTENT,EVENT));
get_anova_table(rt.aov);

lm(meanRT~pid+EVENT:CONTENT,data = myData);

######################################################
# TEST
# Multi-Level Linear Model
# THIS IS NOT WORKING
######################################################
rt.mlm <- lmer(meanRT ~ EVENT*CONTENT + (1|pid), data = myData);
anova(rt.mlm);

##non repeated measure for contrast
rt.lm <- lm(meanRT ~ EVENT*CONTENT,data = myData);
anova(rt.lm);

