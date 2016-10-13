####################
## Load Libraries ##
####################

library(ggplot2)
library(dplyr)
library(reshape2)

#####################
## Load Data Frame ##
#####################

setwd("~/GitHub/Feature-Engineering-Project/Viz Assignment")

df <- read.csv("Tidy_Data_fff_2014j_Dataset.csv")

##################################################
## Create Data Frame of only CMA and TMA Scores ##
##################################################

df1 <- subset(df, select = c(X, TMA, CMA))

## Remove N/A Values ##

df2 <- na.omit(df1)

colnames(df2) <- c("Student_ID","TMA","CMA")

########################################
## Plot average scores on assessments ##
########################################

tma_only <- ggplot(df2, aes(Student_ID,TMA)) + geom_point() + geom_smooth(fill="blue", colour="darkblue", size=1) + ggtitle("Tutor Measured Assessments") + labs(x="Student ID", y="Score")

cma_only <- ggplot(df2, aes(Student_ID,CMA)) + geom_point() + geom_smooth(fill="red", colour="red", size=1) + ggtitle("Computer Measured Assessments") + labs(x="Student ID", y="Score")

########################################
## Plot both assessments on one image ##
########################################

TMA = data.frame(df1$X,df1$TMA)
colnames(TMA) <- c("X","Score")

CMA = data.frame(df1$X,df1$CMA)
colnames(CMA) <- c("X","Score")


visuals = rbind(TMA,CMA)
visuals$Assessment =c(rep("TMA"),rep("CMA"))

cma_and_tma <- ggplot(visuals, aes(X,Score,group=Assessment,col=Assessment)) + geom_point() + geom_smooth()


