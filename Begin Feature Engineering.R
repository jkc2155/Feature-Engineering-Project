setwd("~/GitHub/Project Data Files")

wd_data <- assessments <- read.csv("2014J_data.csv")

# Set Project working directory
setwd("~/GitHub/Feature-Engineering-Project")

############################
### Feature Engineering ###
############################

# Create submission time feature
submission_time <- df_2014j$date.x - df_2014j$date_submitted


# Append submission time to data frame

