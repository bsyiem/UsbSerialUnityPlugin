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
# number of false positives
# number of false negatives
calculateMeansOfObs <- function(mpath,mfiles){
  #number_of_events = 25;
  
  meanRT <- c();
  centralMeanRT <- c();
  peripheralMeanRT <- c();
  false_positives <- c();
  false_negatives <- c();
  
  participantNumbers <- c();
  for(mfile in mfiles){
    mfilePath <- paste(mpath,"/",mfile,sep = "");
    
    data <- read.csv(mfilePath, header = FALSE);
    colnames(data) <- c("ledNumber", "reactionTime", "reactionType");
    
    #get False Negatives ratio (for 25 events)
    #false_positives <- c(false_positives,(nrow(data[(data$reactionType == 'FP'),]))/number_of_events);
    #false_negatives <- c(false_negatives,(nrow(data[(data$reactionType == 'FN'),]))/number_of_events);
    
    #get False Negatives
    false_positives <- c(false_positives,nrow(data[(data$reactionType == 'FP'),]));
    false_negatives <- c(false_negatives,nrow(data[(data$reactionType == 'FN'),]));
    
    
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
  
  meanParticipantReaction<- data.frame(pid = as.factor(participantNumbers), meanRT, peripheralMeanRT, centralMeanRT, false_positives, false_negatives,  stringsAsFactors = FALSE);
  return(meanParticipantReaction);
}

##
# M - Male
# F - Female
addGender <- function(dataFrame,gender){
  dataFrame <- cbind(dataFrame,gender = rep(gender,nrow(dataFrame)));
  return(dataFrame);
}

##
# P - Physical Event
# V - Virtual Event
addEvent <- function(dataFrame,event){
  dataFrame <- cbind(dataFrame,event = rep(event,nrow(dataFrame)));
  return(dataFrame);
}

##
# NC - No Content
# VC - Virtual Content
# VCT - Virtual Content + Task
addContent <- function(dataFrame,content){
  dataFrame <- cbind(dataFrame,content = rep(content,nrow(dataFrame)));
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

#########################################
# summary statistics
#########################################
reactionTime_summary <- myData %>% group_by(content,event) %>% get_summary_stats(meanRT,type = "mean_sd");
reactionTime_summary;

#FN
false_negative_summary <- myData %>% group_by(content,event) %>% get_summary_stats(false_negatives,type = "mean_sd");
false_negative_summary;

#FP
false_positive_summary <- myData %>% group_by(content,event) %>% get_summary_stats(false_positives,type = "mean_sd");
false_positive_summary;

#########################################
# visualizations
#########################################

bxp <- ggboxplot(
  myData, x = "content", y = "meanRT",
  ylab = "RT (seconds)",
  color = "event", palette = "jco"
);
bxp;

bxp2 <- ggboxplot(
  myData, x = "event", y = "meanRT",
  ylab = "RT (seconds)",
  color = "content", palette = "jco"
);
bxp2;

#bar_plot reaction times
p <- ggplot(reactionTime_summary, aes(x = content, y = mean, fill = event)) +
  ylab("RT (milliseconds)") +
  geom_bar(
    stat = "identity",
    position = position_dodge(),
    color = "black",
    width = 0.4
  ) +
  geom_errorbar(
    aes(
      ymin = mean-sd,
      ymax=mean+sd
    ),
    width = 0.2,
    position = position_dodge(0.4)
  );
p;

#########################################
# Assumptions
#########################################
#outlier
myData %>% group_by(content,event) %>% identify_outliers(meanRT);
#shapiro test
myData %>% group_by(content,event) %>% shapiro_test(meanRT);

## since the shapiro-wilk test for VCT content for both events are significant
## we also test using the ks test
#kolmogorov-smirnov test
ks.test(
  myData[((myData$event == "P") & (myData$content == "VCT")),]$meanRT,
  "pnorm",
  mean = mean(myData[((myData$event == "P") & (myData$content == "VCT")),]$meanRT),
  sd = sd(myData[((myData$event == "P") & (myData$content == "VCT")),]$meanRT)
);

ks.test(
  myData[((myData$event == "V") & (myData$content == "VCT")),]$meanRT,
  "pnorm",
  mean = mean(myData[((myData$event == "V") & (myData$content == "VCT")),]$meanRT),
  sd = sd(myData[((myData$event == "V") & (myData$content == "VCT")),]$meanRT)
);

#qqplots
ggqqplot(myData,"meanRT", ggtheme = theme_bw()) + 
  facet_grid(event ~ content, labeller = "label_both");

######################################################
# REPEATED MEASURE ANOVA (Yatani)

# note I am using more than one test for the same calculations
# just to be sure I am getting the correct values
######################################################

#test homogeneity
leveneTest(myData$meanRT~myData$event*myData$content);

## no repeated measure
options(contrasts = c("contr.sum","contr.poly"));
Anova(lm(myData$meanRT~myData$event*myData$content),type = "III");

options(contrasts = c("contr.sum","contr.poly"));
rt.ez <- ezANOVA(data = myData,dv = .(meanRT), wid = .(pid), within = .(event,content), type = 3);
rt.ez;

#this is the same as the above
rt.aov <- aov(meanRT ~ content*event + Error(pid/(content*event)), myData);
summary(rt.aov);

######################################################
# Post Hoc Analysis
######################################################

#----------------------------------------------------#
# effect of event on RT (within event)
#----------------------------------------------------#

#using aov
rt.aov.oneway.contentNC <- aov(meanRT ~ event +Error(pid/event),myData[(myData$content == "NC"),]);
summary(rt.aov.oneway.contentNC);
rt.aov.oneway.contentVC <- aov(meanRT ~ event +Error(pid/event),myData[(myData$content == "VC"),]);
summary(rt.aov.oneway.contentVC);
rt.aov.oneway.contentVCT <- aov(meanRT ~ event +Error(pid/event),myData[(myData$content == "VCT"),]);
summary(rt.aov.oneway.contentVCT);

#using ez package
ezANOVA(data = myData[(myData$content == "NC"),], dv = .(meanRT), wid = .(pid), within = .(event), type = 3);
ezANOVA(data = myData[(myData$content == "VC"),], dv = .(meanRT), wid = .(pid), within = .(event), type = 3);
ezANOVA(data = myData[(myData$content == "VCT"),], dv = .(meanRT), wid = .(pid), within = .(event), type = 3);

#since event only has 1 degree of freedom, a paiwise t test works
pairwise.t.test(
  myData[(myData$content == "NC"),]$meanRT,
  myData[(myData$content == "NC"),]$event,
  paired = TRUE, 
  p.adjust.method = "bonf");
pairwise.t.test(
  myData[(myData$content == "VC"),]$meanRT,
  myData[(myData$content == "VC"),]$event,
  paired = TRUE, 
  p.adjust.method = "bonf");
pairwise.t.test(
  myData[(myData$content == "VCT"),]$meanRT,
  myData[(myData$content == "VCT"),]$event,
  paired = TRUE, 
  p.adjust.method = "bonf");

#using rstatix
pwc <- myData %>% group_by(content) %>% pairwise_t_test(meanRT ~ event, paired = TRUE, p.adjust.method = "bonferroni");
pwc;

#plotting PWC
pwc <- pwc %>% add_xy_position(x = "content");
bxp + 
  color_palette("Dark2") +
  bgcolor("#F5F5F5") +
  grids(linetype = "dashed", color = "white") +
  stat_pvalue_manual(pwc,tip.length = 0,hide.ns = TRUE) +
  labs(
    #    subtitle = "hello",
    caption = get_pwc_label(pwc)
  );

#----------------------------------------------------#
# effect of content on RT (within content)
#----------------------------------------------------#

#using aov
rt.aov.oneway.eventP<- aov(meanRT ~ content +Error(pid/content),myData[(myData$event == "P"),]);
summary(rt.aov.oneway.eventP);
rt.aov.oneway.eventV <- aov(meanRT ~ content +Error(pid/content),myData[(myData$event == "V"),]);
summary(rt.aov.oneway.eventV);

#using ez
ezANOVA(data = myData[(myData$event == "P"),], dv = .(meanRT), wid = .(pid), within = .(content), type = 3);
ezANOVA(data = myData[(myData$event == "V"),], dv = .(meanRT), wid = .(pid), within = .(content), type = 3);

# Both are significant so we will move on to pairwise t tests

#using pairwiset.tests
pairwise.t.test(
  myData[(myData$event == "P"),]$meanRT,
  myData[(myData$event == "P"),]$content,
  paired =TRUE,
  p.adjust.method = "bonf"
);

pairwise.t.test(
  myData[(myData$event == "V"),]$meanRT,
  myData[(myData$event == "V"),]$content,
  paired =TRUE,
  p.adjust.method = "bonf"
);

#using rstatix
pwc2 <- myData %>% group_by(event) %>% pairwise_t_test(meanRT ~ content, paired = TRUE, p.adjust.method = "bonferroni");
pwc2;

#plotting PWC2
pwc2 <- pwc2 %>% add_xy_position(x = "event");
bxp2 +
  color_palette("Dark2") +
#  color_palette(c("#FF5A00","#55AAAA", "#0000FF")) +
  bgcolor("#F5F5F5") +
  grids(linetype = "dashed", color = "#ECECEC") +
  stat_pvalue_manual(pwc2,tip.length = 0,hide.ns = TRUE) +
  labs(
#    subtitle = "hello",
    caption = get_pwc_label(pwc2)
  );

######################################################
# central vs peripheral t-test
######################################################

myDataP <- myData[(myData$event == "P"),];
myDataP <- gather(myDataP,position, pc_rt,peripheralMeanRT,centralMeanRT,factor_key = TRUE);

myDataP %>% group_by(content,position) %>% get_summary_stats(pc_rt,type = "mean_sd");

pwc3 <- myDataP %>% group_by(content) %>% pairwise_t_test(pc_rt~position , paired = TRUE, p.adjust.method = "bonferroni");
pwc3;
######################################################
# TEST
# REPEATED MEASURE ANOVA (rstatix)
# THIS IS NOT WORKING
######################################################

rt.aov <- anova_test(data = myData,dv = meanRT, wid = pid, within = c(content,event));
get_anova_table(rt.aov);

lm(meanRT~pid+event:content,data = myData);

######################################################
# TEST
# Multi-Level Linear Model
# THIS IS NOT WORKING
######################################################
rt.mlm <- lmer(meanRT ~ event*content + (1|pid), data = myData);
anova(rt.mlm);

##non repeated measure for contrast
rt.lm <- lm(meanRT ~ event*content,data = myData);
anova(rt.lm);

##################################################################################################################################################################
# FALSE NEGATIVES - MISSED EVENTS
#########################################
# Assumptions
#########################################
#outlier
myData %>% group_by(content,event) %>% identify_outliers(false_negatives);
#shapiro test
myData %>% group_by(content,event) %>% shapiro_test(false_negatives);

#qqplots
ggqqplot(myData,"false_negatives", ggtheme = theme_bw()) + 
  facet_grid(event ~ content, labeller = "label_both");

hist(myData[(myData$event == "P") & (myData$content == "NC"),]$false_negatives);
hist(myData[(myData$event == "P") & (myData$content == "VC"),]$false_negatives);
hist(myData[(myData$event == "P") & (myData$content == "VCT"),]$false_negatives);
hist(myData[(myData$event == "V") & (myData$content == "NC"),]$false_negatives);
hist(myData[(myData$event == "V") & (myData$content == "VC"),]$false_negatives);
hist(myData[(myData$event == "V") & (myData$content == "VCT"),]$false_negatives);

###########################
# Cannot assume normality 
# follow with friedman tests
# since friendman can only compare data across one factor, we will run it twice over EVENT and CONTENT

myDataFN <- subset(myData, select = -c(peripheralMeanRT,centralMeanRT,meanRT, gender, false_positives));

# effect of  EVENT on false_negatives
myDataFN_by_event <- spread(myDataFN,event,false_negatives);

myDataFN_by_event_matrix <- as.matrix(subset(myDataFN_by_event, select = -c(pid,content)));

friedman.test(myDataFN_by_event_matrix);
#friedman.test(false_negatives~event | content, data = myData);

# effect of CONTENT on false_negatives
myDataFN_by_content <- spread(myDataFN,content,false_negatives);

myDataFN_by_content_matrix <- as.matrix(subset(myDataFN_by_content, select = -c(pid,event)));

friedman.test(myDataFN_by_content_matrix);

##################################################
# takes each condition as a group.
# will not account for varibility within factors
# should not be considered.
###################################################

#BY CONDITION i.e., all 6 conditions
# separating data by each participant
# treating each condition as a group level i.e., 6 levels
myDataFN_by_participant <- pivot_wider(
                            data = myDataFN,
                            id_cols = pid,
                            names_from = c(event,content),
                            values_from = false_negatives);

#removing pids for conversion to matrix for friedman test.
myDataFN_matrix <- as.matrix(subset(myDataFN_by_participant, select = -c(pid)));

friedman.test(myDataFN_matrix);

## 

#since we found significant effect of event on FN via the friedman test
#we follow up with a wilcox test. 
#(this should give similar results to friedman).

pairwise.wilcox.test(myData$false_negatives,myData$event, p.adj = "bonferroni", exact = F, paired = T);


########################
# Attempted Data Transformation

myDataFN$false_negatives <- sqrt(myDataFN$false_negatives + 0.5);

myDataFN

hist(myDataFN[(myDataFN$event == "P") & (myDataFN$content == "NC"),]$false_negatives);
hist(myDataFN[(myDataFN$event == "P") & (myDataFN$content == "VC"),]$false_negatives);
hist(myDataFN[(myDataFN$event == "P") & (myDataFN$content == "VCT"),]$false_negatives);
hist(myDataFN[(myDataFN$event == "V") & (myDataFN$content == "NC"),]$false_negatives);
hist(myDataFN[(myDataFN$event == "V") & (myDataFN$content == "VC"),]$false_negatives);
hist(myDataFN[(myDataFN$event == "V") & (myDataFN$content == "VCT"),]$false_negatives);

options(contrasts = c("contr.sum","contr.poly"));
rt.ez <- ezANOVA(data = myDataFN,dv = .(false_negatives), wid = .(pid), within = .(event,content), type = 3);
rt.ez;



