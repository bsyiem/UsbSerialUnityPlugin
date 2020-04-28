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
addGender <- function(dataFrame,GENDER){
  dataFrame <- cbind(dataFrame,GENDER = rep(GENDER,nrow(dataFrame)));
  return(dataFrame);
}

##
# P - Physical Event
# V - Virtual Event
addEvent <- function(dataFrame,EVENT_TYPE){
  dataFrame <- cbind(dataFrame,EVENT_TYPE = rep(EVENT_TYPE,nrow(dataFrame)));
  return(dataFrame);
}

##
# NC - No Blocks
# VC - Virtual Blocks
# VCT - Virtual Blocks + Task
addContent <- function(dataFrame,CONTENT){
  dataFrame <- cbind(dataFrame,CONTENT = rep(CONTENT,nrow(dataFrame)));
  return(dataFrame);
}

#######
#AGE
#######

addAge <- function(mdata){
  pid_for_age <- as.factor(c(1:13,15:21));
  age <- c(18,32,23,19,19,24,24,20,24,25,29,32,26,22,25,27,32,32,27,24);
  age.df <- data.frame(pid = pid_for_age,age);
  mdata <- inner_join(mdata,age.df,by = "pid", keep = F);
  #mdata <- subset(mdata, select = -c(age.x));
  #rename(mdata,c("age.y" = "age"));
  return(mdata);
}


#add actual and participant ball pass counts
addCount <- function(mdata){
  pid_for_count <- as.factor(rep(c(1:13,15:21),each = 2));
  EVENT_TYPE <- as.factor(rep(c("P","V"),times = 20));
  CONTENT <- as.factor(rep("VCT",times = 40));
  actual_count <- c(65,62,
                    65,56,
                    57,61,
                    71,63,
                    57,75,
                    69,64,
                    56,63,
                    74,68,
                    64,66,
                    65,60,
                    66,61,
                    60,67,
                    54,55,
                    61,62,
                    66,64,
                    65,63,
                    59,65,
                    70,56,
                    70,69,
                    60,71);
  participant_count <- c(66,63,
                         65,58,
                         67,60,
                         71,63,
                         56,75,
                         70,68,
                         55,67,
                         69,67,
                         62,59,
                         63,60,
                         65,60,
                         63,69,
                         52,52,
                         59,63,
                         56,50,
                         65,66,
                         58,68,
                         65,60,
                         72,70,
                         59,64);
  count.df <- data.frame(pid = pid_for_count,EVENT_TYPE, CONTENT, participant_count,actual_count);
  
  mdata <- inner_join(mdata,count.df,by = c("pid","EVENT_TYPE","CONTENT"), keep = F);
  return(mdata);
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

#means for all GENDERs
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

myData <- addAge(myData);
myData$pid = as.factor(myData$pid);

myDataCount <- addCount(myData);
myDataCount$pid = as.factor(myDataCount$pid);
myDataCount$CONTENT = as.factor(myDataCount$CONTENT);


##
# changing data use for plotting 
# spelled out conditions
# milliseconds instead of microseconds
myData[,2:4] <- myData[,2:4]/1000 ;

##spelling out acronyms
revalue(myData$GENDER, c("M" = "Male")) -> myData$GENDER;
revalue(myData$GENDER, c("F" = "Female")) -> myData$GENDER;

revalue(myData$EVENT_TYPE, c("P" = "Physical")) -> myData$EVENT_TYPE;
revalue(myData$EVENT_TYPE, c("V" = "Virtual")) -> myData$EVENT_TYPE;

revalue(myData$CONTENT, c("NC" = "None")) -> myData$CONTENT;
revalue(myData$CONTENT, c("VC" = "Blocks")) -> myData$CONTENT;
revalue(myData$CONTENT, c("VCT" = "Blocks + Task")) -> myData$CONTENT;


#########################################
# summary statistics
#########################################
reactionTime_summary <- myData %>% group_by(CONTENT,EVENT_TYPE) %>% get_summary_stats(meanRT,type = "mean_sd");
reactionTime_summary;

#FN
false_negative_summary <- myData %>% group_by(CONTENT,EVENT_TYPE) %>% get_summary_stats(false_negatives,type = "mean_sd");
false_negative_summary;

#FP
false_positive_summary <- myData %>% group_by(CONTENT,EVENT_TYPE) %>% get_summary_stats(false_positives,type = "mean_sd");
false_positive_summary;

#Age
myData %>% distinct(pid,age) %>% subset(select = "age") %>% get_summary_stats();

#count error
counts <- myDataCount %>% 
  distinct(pid,EVENT_TYPE,CONTENT,actual_count,participant_count);
counts$countError <- counts$actual_count - counts$participant_count;

get_summary_stats(counts);
#########################################
# visualizations
#########################################

#box plots reaction times
bxp <- ggboxplot(
  myData, x = "CONTENT", y = "meanRT",
  ylab = "RT (milliseconds)",
  color = "black",
  fill = "EVENT_TYPE",
  palette = "jco"
);
bxp;

bxp2 <- ggboxplot(
  myData, x = "EVENT_TYPE", y = "meanRT",
  ylab = "RT (milliseconds)",
  color = "black",
  fill = "CONTENT",
  palette = "jco"
);
bxp2;


#bar_plot reaction times
p <- ggplot(reactionTime_summary, aes(x = CONTENT, y = mean, fill = EVENT_TYPE)) +
  theme_minimal() +
  font("caption", size = 14) +
  font("xy.text", size = 16) +
  font("legend.title", size = 18, face = "bold") +
  font("legend.text",size = 16) +
  font("xlab", size = 18, face = "bold") +
  font("ylab", size = 18, face = "bold") +
  ylab("RT (milliseconds)") +
  scale_fill_brewer(palette = "Pastel2") +
  #labs(title= "Reaction time by CONTENT levels") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top"
        ) +
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
myData %>% group_by(CONTENT,EVENT_TYPE) %>% identify_outliers(meanRT);
#shapiro test
myData %>% group_by(CONTENT,EVENT_TYPE) %>% shapiro_test(meanRT);

## since the shapiro-wilk test for VCT CONTENT for both EVENT_TYPEs are significant
## we also test using the ks test
#kolmogorov-smirnov test
ks.test(
  myData[((myData$EVENT_TYPE == "Physical") & (myData$CONTENT == "Blocks + Task")),]$meanRT,
  "pnorm",
  mean = mean(myData[((myData$EVENT_TYPE == "Physical") & (myData$CONTENT == "Blocks + Task")),]$meanRT),
  sd = sd(myData[((myData$EVENT_TYPE == "Physical") & (myData$CONTENT == "Blocks + Task")),]$meanRT)
);

ks.test(
  myData[((myData$EVENT_TYPE == "Virtual") & (myData$CONTENT == "Blocks + Task")),]$meanRT,
  "pnorm",
  mean = mean(myData[((myData$EVENT_TYPE == "Virtual") & (myData$CONTENT == "Blocks + Task")),]$meanRT),
  sd = sd(myData[((myData$EVENT_TYPE == "Virtual") & (myData$CONTENT == "Blocks + Task")),]$meanRT)
);

#qqplots
ggqqplot(myData,"meanRT", ggtheme = theme_bw()) + 
  facet_grid(EVENT_TYPE ~ CONTENT, labeller = "label_both");

######################################################
# REPEATED MEASURE ANOVA (Yatani)

# note I am using more than one test for the same calculations
# just to be sure I am getting the correct values
######################################################

#test homogeneity
leveneTest(myData$meanRT~myData$EVENT_TYPE*myData$CONTENT);

## no repeated measure
options(contrasts = c("contr.sum","contr.poly"));
Anova(lm(myData$meanRT~myData$EVENT_TYPE*myData$CONTENT),type = "III");

options(contrasts = c("contr.sum","contr.poly"));
rt.ez <- ezANOVA(data = myData,dv = .(meanRT), wid = .(pid), within = .(EVENT_TYPE,CONTENT), type = 3, detailed = T);
rt.ez;

#calculate effect sizes
rt.ez$ANOVA$SSn/(rt.ez$ANOVA$SSn+rt.ez$ANOVA$SSd);

#this is the same as the above
rt.aov <- aov(meanRT ~ CONTENT*EVENT_TYPE + Error(pid/(CONTENT*EVENT_TYPE)), myData);
summary(rt.aov);

######################################################
# Post Hoc Analysis
######################################################


#----------------------------------------------------#
# effect of EVENT_TYPE on RT (within EVENT_TYPE)
#----------------------------------------------------#

#using aov
rt.aov.oneway.CONTENTNC <- aov(meanRT ~ EVENT_TYPE +Error(pid/EVENT_TYPE),myData[(myData$CONTENT == "None"),]);
summary(rt.aov.oneway.CONTENTNC);
rt.aov.oneway.CONTENTVC <- aov(meanRT ~ EVENT_TYPE +Error(pid/EVENT_TYPE),myData[(myData$CONTENT == "Blocks"),]);
summary(rt.aov.oneway.CONTENTVC);
rt.aov.oneway.CONTENTVCT <- aov(meanRT ~ EVENT_TYPE +Error(pid/EVENT_TYPE),myData[(myData$CONTENT == "Blocks + Task"),]);
summary(rt.aov.oneway.CONTENTVCT);

#using ez package
ez.nc <- ezANOVA(data = myData[(myData$CONTENT == "None"),], dv = .(meanRT), wid = .(pid), within = .(EVENT_TYPE), type = 3, detailed = T);
ez.nc;
ez.nc$ANOVA$SSn/(ez.nc$ANOVA$SSn+ez.nc$ANOVA$SSd);

ez.vc <- ezANOVA(data = myData[(myData$CONTENT == "Blocks"),], dv = .(meanRT), wid = .(pid), within = .(EVENT_TYPE), type = 3, detailed = T);
ez.vc;
ez.vc$ANOVA$SSn/(ez.vc$ANOVA$SSn+ez.vc$ANOVA$SSd);

ez.vct <- ezANOVA(data = myData[(myData$CONTENT == "Blocks + Task"),], dv = .(meanRT), wid = .(pid), within = .(EVENT_TYPE), type = 3, detailed = T);
ez.vct;
ez.vct$ANOVA$SSn/(ez.vct$ANOVA$SSn+ez.vct$ANOVA$SSd);

#since EVENT_TYPE only has 1 degree of freedom, a paiwise t test works
pairwise.t.test(
  myData[(myData$CONTENT == "None"),]$meanRT,
  myData[(myData$CONTENT == "None"),]$EVENT_TYPE,
  paired = TRUE, 
  p.adjust.method = "bonf");
pairwise.t.test(
  myData[(myData$CONTENT == "Blocks"),]$meanRT,
  myData[(myData$CONTENT == "Blocks"),]$EVENT_TYPE,
  paired = TRUE, 
  p.adjust.method = "bonf");
pairwise.t.test(
  myData[(myData$CONTENT == "Blocks + Task"),]$meanRT,
  myData[(myData$CONTENT == "Blocks + Task"),]$EVENT_TYPE,
  paired = TRUE, 
  p.adjust.method = "bonf");

#using rstatix
pwc <- myData %>% group_by(CONTENT) %>% pairwise_t_test(meanRT ~ EVENT_TYPE, paired = TRUE, p.adjust.method = "bonferroni");
pwc;

#cohens d effect size
myData %>% group_by(CONTENT) %>% cohens_d(meanRT~EVENT_TYPE,paired = T)

#plotting PWC
pwc <- pwc %>% add_xy_position(x = "CONTENT");
bxp + 
  font("caption", size = 14) +
  font("xy.text", size = 16) +
  font("legend.title", size = 18, face = "bold") +
  font("legend.text",size = 16) +
  font("xlab", size = 18, face = "bold") +
  font("ylab", size = 18, face = "bold") +
  #color_palette("Dark2") +
  scale_fill_brewer(palette = "Pastel2") +
  #scale_fill_manual(values = c("#999999","#E69F00","#56B4E9")) +
  #bgcolor("#F5F5F5") +
  #grids(linetype = "dashed", color = "white") +
  stat_pvalue_manual(pwc,label = "p.adj",tip.length = 0.01,hide.ns = TRUE,size = 4.5) +
  labs(
    #    subtitle = "hello",
    caption = get_pwc_label(pwc)
  );

#----------------------------------------------------#
# effect of CONTENT on RT (within CONTENT)
#----------------------------------------------------#

#using aov
rt.aov.oneway.EVENT_TYPEP<- aov(meanRT ~ CONTENT +Error(pid/CONTENT),myData[(myData$EVENT_TYPE == "Physical"),]);
summary(rt.aov.oneway.EVENT_TYPEP);
rt.aov.oneway.EVENT_TYPEV <- aov(meanRT ~ CONTENT +Error(pid/CONTENT),myData[(myData$EVENT_TYPE == "Virtual"),]);
summary(rt.aov.oneway.EVENT_TYPEV);

#using ez
ez.p <- ezANOVA(data = myData[(myData$EVENT_TYPE == "Physical"),], dv = .(meanRT), wid = .(pid), within = .(CONTENT), type = 3, detailed = T);
ez.p;
ez.p$ANOVA$SSn/(ez.p$ANOVA$SSn+ez.p$ANOVA$SSd);

ez.r <- ezANOVA(data = myData[(myData$EVENT_TYPE == "Virtual"),], dv = .(meanRT), wid = .(pid), within = .(CONTENT), type = 3, detailed = T);
ez.r;
ez.r$ANOVA$SSn/(ez.r$ANOVA$SSn+ez.r$ANOVA$SSd);

# Both are significant so we will move on to pairwise t tests

#using pairwiset.tests
pairwise.t.test(
  myData[(myData$EVENT_TYPE == "Physical"),]$meanRT,
  myData[(myData$EVENT_TYPE == "Physical"),]$CONTENT,
  paired =TRUE,
  p.adjust.method = "bonf"
);

pairwise.t.test(
  myData[(myData$EVENT_TYPE == "Virtual"),]$meanRT,
  myData[(myData$EVENT_TYPE == "Virtual"),]$CONTENT,
  paired =TRUE,
  p.adjust.method = "bonf"
);

#using rstatix
pwc2 <- myData %>% group_by(EVENT_TYPE) %>% pairwise_t_test(meanRT ~ CONTENT, paired = TRUE, p.adjust.method = "bonferroni");
pwc2;

#cohens d effect size
myData %>% group_by(EVENT_TYPE) %>% cohens_d(meanRT~CONTENT,paired = T)

# does not work -> does not adjust with bonferroni
#compare_means(meanRT ~ CONTENT,paired = TRUE,method = "t.test", group.by = "EVENT_TYPE", data = myData, p.adjust.method = "bonferroni");

#plotting PWC2
pwc2 <- pwc2 %>% add_xy_position(x = "EVENT_TYPE");

# <- pwc2 %>% p_format(p.adj, accuracy = 0.0001);

bxp2 +
  font("caption", size = 14) +
  font("xy.text", size = 16) +
  font("legend.title", size = 18, face = "bold") +
  font("legend.text",size = 16) +
  font("xlab", size = 18, face = "bold") +
  font("ylab", size = 18, face = "bold") +
  #color_palette("Dark2") +
  scale_fill_brewer(palette = "Pastel2") +
  #scale_fill_manual(values = c("#999999","#E69F00","#56B4E9")) +
  #bgcolor("#F5F5F5") +
  #grids(linetype = "dashed", color = "white") +
  stat_pvalue_manual(pwc2,label = "p.adj",tip.length = 0.01,hide.ns = TRUE,size = 4.5) +
  labs(
    #    subtitle = "hello",
    caption = get_pwc_label(pwc2)
  );

######################################################
# TEST
# REPEATED MEASURE ANOVA (rstatix)
# THIS IS NOT WORKING
######################################################

rt.aov <- anova_test(data = myData,dv = meanRT, wid = pid, within = c(CONTENT,EVENT_TYPE));
get_anova_table(rt.aov);

lm(meanRT~pid+EVENT_TYPE:CONTENT,data = myData);

######################################################
# TEST
# Multi-Level Linear Model
# THIS IS NOT WORKING
######################################################
rt.mlm <- lmer(meanRT ~ EVENT_TYPE*CONTENT + (1|pid), data = myData);
anova(rt.mlm);

##non repeated measure for contrast
rt.lm <- lm(meanRT ~ EVENT_TYPE*CONTENT,data = myData);
anova(rt.lm);

##################################################################################################################################################################
# FALSE NEGATIVES - MISSED EVENT_TYPES
#########################################
# Assumptions
#########################################
#outlier
myData %>% group_by(CONTENT,EVENT_TYPE) %>% identify_outliers(false_negatives);
#shapiro test
myData %>% group_by(CONTENT,EVENT_TYPE) %>% shapiro_test(false_negatives);

#qqplots
ggqqplot(myData,"false_negatives", ggtheme = theme_bw()) + 
  facet_grid(EVENT_TYPE ~ CONTENT, labeller = "label_both");

###########################
# Cannot assume normality 
# follow with friedman tests
# since friendman can only compare data across one factor, we will run it twice over EVENT_TYPE and CONTENT

myDataFN <- subset(myData, select = -c(peripheralMeanRT,centralMeanRT,meanRT, GENDER, false_positives));

# effect of  EVENT_TYPE on false_negatives
myDataFN_by_EVENT_TYPE <- spread(myDataFN,EVENT_TYPE,false_negatives);

myDataFN_by_EVENT_TYPE_matrix <- as.matrix(subset(myDataFN_by_EVENT_TYPE, select = -c(pid,CONTENT)));

friedman.test(myDataFN_by_EVENT_TYPE_matrix);
#friedman.test(false_negatives~EVENT_TYPE | CONTENT, data = myData);

# effect of CONTENT on false_negatives
myDataFN_by_CONTENT <- spread(myDataFN,CONTENT,false_negatives);

myDataFN_by_CONTENT_matrix <- as.matrix(subset(myDataFN_by_CONTENT, select = -c(pid,EVENT_TYPE)));

friedman.test(myDataFN_by_CONTENT_matrix);

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
  names_from = c(EVENT_TYPE,CONTENT),
  values_from = false_negatives);

#removing pids for conversion to matrix for friedman test.
myDataFN_matrix <- as.matrix(subset(myDataFN_by_participant, select = -c(pid)));

friedman.test(myDataFN_matrix);

## 

#since we found significant effect of EVENT_TYPE on FN via the friedman test
#we follow up with a wilcox test. 
#(this should give similar results to friedman).

pairwise.wilcox.test(myData$false_negatives,myData$EVENT_TYPE, p.adj = "bonferroni", exact = F, paired = T);


########################
# Attempted Data Transformation

myDataFN$false_negatives <- sqrt(myDataFN$false_negatives + 0.5);

myDataFN

hist(myDataFN[(myDataFN$EVENT_TYPE == "Physical") & (myDataFN$CONTENT == "None"),]$false_negatives);
hist(myDataFN[(myDataFN$EVENT_TYPE == "Physical") & (myDataFN$CONTENT == "Blocks"),]$false_negatives);
hist(myDataFN[(myDataFN$EVENT_TYPE == "Physical") & (myDataFN$CONTENT == "Blocks + Task"),]$false_negatives);
hist(myDataFN[(myDataFN$EVENT_TYPE == "Virtual") & (myDataFN$CONTENT == "None"),]$false_negatives);
hist(myDataFN[(myDataFN$EVENT_TYPE == "Virtual") & (myDataFN$CONTENT == "Blocks"),]$false_negatives);
hist(myDataFN[(myDataFN$EVENT_TYPE == "Virtual") & (myDataFN$CONTENT == "Blocks + Task"),]$false_negatives);

options(contrasts = c("contr.sum","contr.poly"));
rt.ez <- ezANOVA(data = myDataFN,dv = .(false_negatives), wid = .(pid), within = .(EVENT_TYPE,CONTENT), type = 3, detailed = T);
rt.ez;




