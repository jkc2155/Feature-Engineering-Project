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

apply(df,2,function(x) sum(is.na(x)))

#######################################################################
## Create new Data frame with only features that have missing values ##
#######################################################################

df1 <- subset(df, select = -c(id_student, homepage, gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, studied_credits, disability, final_result, date_registration.x, date_registration.y, date_unregistration.x, date_unregistration.y))

###################################
## Create missing value function ##
###################################

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=1)) + 
    labs(x = "Variables in Dataset",
         y = "Number of Observations")
}

#########################
## Plot Missing Values ##
#########################

## This function shows the severity of missing values for each feature.

ggplot_missing(df1)

## The heatmap function clusters rows and columns, in this case by missingness. 

heatmap(is.na(df1)*1, scale='none')

