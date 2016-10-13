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
df3 <- df1

##############################
## Plot and Remove Outliers ##
##############################

outlierTMA <- function(df3, var) {
  TMA <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(TMA))
  m1 <- mean(TMA, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(TMA, main="With outliers")
  hist(TMA, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(TMA)$out
  mo <- mean(outlier)
  TMA <- ifelse(TMA %in% outlier, NA, TMA)
  boxplot(TMA, main="Without outliers")
  hist(TMA, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(TMA))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(TMA))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(TMA, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(TMA)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(TMA))
  }
}

outlierCMA <- function(df3, var) {
  CMA <- eval(substitute(var),eval(df3))
  na1 <- sum(is.na(CMA))
  m1 <- mean(CMA, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(CMA, main="With outliers")
  hist(CMA, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(CMA)$out
  mo <- mean(outlier)
  CMA <- ifelse(CMA %in% outlier, NA, CMA)
  boxplot(CMA, main="Without outliers")
  hist(CMA, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(CMA))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(CMA))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(CMA, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    df3[as.character(substitute(var))] <- invisible(CMA)
    assign(as.character(as.list(match.call())$df3), df3, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(df3))
  } else{
    cat("Nothing changed", "n")
    return(invisible(CMA))
  }
}

outlierTMA(df3, TMA)
outlierCMA(df3, CMA)
y

########################################
## Plot average scores on assessments ##
########################################

tma_only_no_outliers <- ggplot(df3, aes(X,TMA)) + geom_point() + geom_smooth(fill="blue", colour="darkblue", size=1) + ggtitle("Tutor Measured Assessments - No Outliers") + labs(x="Student ID", y="Score")

cma_only_no_outliers <- ggplot(df3, aes(X,CMA)) + geom_point() + geom_smooth(fill="red", colour="red", size=1) + ggtitle("Computer Measured Assessments - No Outliers") + labs(x="Student ID", y="Score")

