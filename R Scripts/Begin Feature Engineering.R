setwd("~/GitHub/Project Data Files")

wd_data <- assessments <- read.csv("2014J_data.csv")

# Set Project working directory
setwd("~/GitHub/Feature-Engineering-Project")

######################
### Tidy your data ###
######################

library(tidyr)

# Figure out how to spread the data
wd_data_2 <- spread(wd_data, total_click, sum_click)


############################
### Feature Engineering ###
############################

# Create submission time feature
submission_time <- df_2014j$date.x - df_2014j$date_submitted


# Append submission time to data frame

