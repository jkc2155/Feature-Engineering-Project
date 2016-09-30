setwd("~/GitHub/Project Data Files")

wd_data <- assessments <- read.csv("2014J_data.csv")

# Set Project working directory
setwd("~/GitHub/Feature-Engineering-Project")

########################################
### Tidy your data using Sample Data ###
########################################

library(tidyr)

# Select student ID to test
student_101217 <- subset(wd_data, id_student %in% "101217")
student_552173 <- subset(wd_data, id_student %in% "552173")
student_655386 <- subset(wd_data, id_student %in% "655386")

# Merge sample student data together
student_test_data <- rbind(student_655386,student_552173,student_101217)

#############################################
### Feature Engineering Using Sample Data ###
#############################################

# Create submission time feature
test_data_submission_time <- student_test_data$date.x - student_test_data$date_submitted

# Count number of entrys
count_of_entrys <- within(student_test_data, { count <- ave(id_student, FUN=function(x) length(unique(x)))})
names(count_of_entrys) <- c("id_student","number_of_entries")

# Average score of assessment type

# Average number of clicks per activity type


# Spread data using tidyr






