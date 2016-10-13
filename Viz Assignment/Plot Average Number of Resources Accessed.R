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

############################################
## Check for missing values in data frame ##
############################################