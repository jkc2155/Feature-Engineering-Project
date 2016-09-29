# Load Libraries
library(tidyr)
library(neuralnet)

# Set working directory to data folder
setwd("~/GitHub/Project Data Files")

# Load .CSV files and convert to dataframes
assessments <- read.csv("assessments.csv")
courses <- read.csv("courses.csv")
std_assessments <- read.csv("studentAssessment.csv")
std_info <- read.csv("studentinfo.csv")
std_registration <- read.csv("studentRegistration.csv")
std_vle <- read.csv("studentVle.csv")
vle <- read.csv("vle.csv")

# Set Project working directory
setwd("~/GitHub/Feature-Engineering-Project")

# Create new data frame by combining Assessment data with Student Assessment data
combined_assessment <- merge(std_assessments, assessments, by = "id_assessment", all = TRUE)

# Isolate specific course to analyze
table(combined_assessment$code_module)
# Table indicates course "FFF" has highest number of rows at 54k

# Create assessment data from course FFF
FFF_Assessment <- subset(combined_assessment, code_module %in% "FFF")

# Create working data frame
df <- FFF_Assessment

## Begin Feature Engineering on assessment Data
# Create feature of time between assisgnemtn due date and time of actual submission
submission_time <- df$date - df$date_submitted

# Append new features to data frame
df1 <- data.frame(df,submission_time)

# Begin feature selection
df2 <- data.frame(df1$score, df1$assessment_type, df1$weight, df1$submission_time)

# Rename columns
names(df2) <- c("score","assessment_type","weight","submission_time")

# Check for missing values in data set
apply(df2,2,function(x) sum(is.na(x)))

# Remove rows with missing values
df3 <- na.omit(df2)

# confirm missing values are removed
apply(df3,2,function(x) sum(is.na(x)))

# Split data into training and test set at 3/4ths split
index <- sample(1:nrow(df3),round(0.75*nrow(df3)))
train <- df3[index,]
test <- df3[-index,]

# Begin data preprocessing
# Normalize data set



