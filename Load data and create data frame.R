# Load Libraries
library(tidyr)
library(neuralnet)
library(clusterSim)

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

# Begin data preprocessing
# Convert features to numeric values
df3$score <- as.numeric(df3$score)
df3$weight <- as.numeric(df3$weight)
df3$submission_time <- as.numeric(df3$submission_time)

# Apply z-score normalization to numeric values in dataset
z_score <- (df3$score - mean(df3$score)) / sd(df3$score)
z_weight <- (df3$weight - mean(df3$weight)) / sd(df3$weight)
z_submission_time <- (df3$submission_time - mean(df3$submission_time)) / sd(df3$submission_time)

# Create normalized data frame
df4 <- data.frame(z_score, df3$assessment_type, z_weight, z_submission_time)

# Rename columns
names(df4) <- c("score","assessment_type","weight","submission_time")

# Convert categorical values to boolean inputs
flags <- data.frame(Reduce(cbind, lapply(levels(df4$assessment_type), function(x){(df4$assessment_type== x)*1})))

# Bind boolean variables to new dataset
df5 <- data.frame(df4$score, df4$weight,df4$submission_time, flags)

# Rename dataset values
names(df5) <- c("score","weight","submission_time","CMA","Exam","TMA")

# Split data into training and test set at 3/4ths split then we fit a linear regression model and test it on the test set. 
index <- sample(1:nrow(df5),round(0.75*nrow(df5)))
train <- df5[index,]
test <- df5[-index,]
lm.fit <- glm(score~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$score)^2)/nrow(test)

# fit neural net
n <- names(train)
f <- as.formula(paste("score ~", paste(n[!n %in% "score"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)
