####################
## Load Libraries ##
####################

library(tidyr)
library(dplyr)

#################################
## Load Data Frames from Files ##
#################################

# Set working directory to data folder
setwd("~/GitHub/Project Data Files")

# Load .CSV files and convert to dataframes
assessments_df <- read.csv("assessments.csv")
courses_df <- read.csv("courses.csv")
std_assessments_df <- read.csv("studentAssessment.csv")
std_info_df <- read.csv("studentinfo.csv")
std_registration_df <- read.csv("studentRegistration.csv")
std_vle_df <- read.csv("studentVle.csv")
vle_df <- read.csv("vle.csv")

# Set Project working directory
setwd("~/GitHub/Feature-Engineering-Project")

###################################
#### Begin Data Processing ####
###################################

## Create data frames for semester 2014j of course "FFF." ## 

assessment_fff_2014j <- subset(assessments_df, code_module %in% "FFF" & code_presentation %in% "2014J", select = c(id_assessment, assessment_type, date, weight))

std_info_df_fff_2014j <- subset(std_info_df, code_module %in% "FFF" & code_presentation %in% "2014J", select = c(id_student, gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, studied_credits, disability, final_result))

std_registration_fff_2014j <- subset(std_registration_df, code_module %in% "FFF" & code_presentation %in% "2014J", select = c(id_student, date_registration, date_unregistration))

std_vle_fff_2014j <- subset(std_vle_df, code_module %in% "FFF" & code_presentation %in% "2014J", select = c(id_student, id_site, date, sum_click))

vle_fff_2014j <- subset(vle_df, code_module %in% "FFF" & code_presentation %in% "2014J", select = c(id_site, activity_type))


########################
## Feature Generation ##
########################

# Create new data frame by combining Assessment data with Student Assessment data
combined_assessments_fff_2014j <- merge(std_assessments_df, assessment_fff_2014j, by = "id_assessment", all = FALSE)

# Create new data frame by combining Assessment data with Student VLE data
std_assessment_vle_fff_2014j <- merge(combined_assessments_fff_2014j, std_vle_fff_2014j, by= "id_student")

# Create new data frame by student vle information data with vle discription data
std_assessment_vle_fff_2014j <- merge(std_assessment_vle_fff_2014j, vle_fff_2014j, by= "id_site")

# Create data frame of average score for specific assessment type of each student. 
assessment_score_avg_fff_2014j <- std_assessment_vle_fff_2014j %>% dplyr::group_by(assessment_type, id_student) %>% dplyr::summarise(mean(score))

# Create table with average number of clicks per student and activity type
activity_type_sum_click_fff_2014j <- std_assessment_vle_fff_2014j %>% dplyr::group_by(activity_type, id_student) %>% dplyr::summarise(mean(sum_click))

# Spread new tables
spread_assessment_core_avg_fff_2014j <- tidyr::spread(assessment_score_avg_fff_2014j, assessment_type, 'mean(score)')

spread_activity_type_avg_click_fff_2014j <- tidyr::spread(activity_type_sum_click_fff_2014j, activity_type, 'mean(sum_click)')

# Merge student data and average data and feature together
df <- merge(spread_activity_type_avg_click_fff_2014j, spread_assessment_core_avg_fff_2014j, by = "id_student")
df <- merge(df, std_info_df_fff_2014j, by ="id_student")
df <- merge(df, std_registration_fff_2014j, by = "id_student")

##################################
## Prepare Data.frame for model ##
##################################

# Check for missing values in data frame
apply(df,2,function(x) sum(is.na(x)))

# Begin feature selection and remove uneeded columns from dataframe
df1 <- subset(df, select = -c(date_unregistration, date_registration) )

# Replace missing values in data frame with "0"
df1[is.na(df1)] <- 0

# Remove categorical values from dataset
df2 <- subset(df1, select = -c(final_result, disability, age_band, imd_band, highest_education, region, gender))

# Convert values to numeric
df2 <- sapply(df2, as.numeric )

# Normalize numeric values
df2 <- data.frame(scale(df2, center = TRUE, scale = TRUE))

######################################################
## Append normalized columns to categorical columns ##
######################################################

df3 <- data.frame(df2, df1$final_result, df1$disability, df1$age_band, df1$imd_band, df1$highest_education, df1$region, df1$gender)

names(df3) <- c("id_student","dataplus","dualpane","forumng","glossary","homepage","htmlactivity","oucollaborate","oucontent","ouwiki","page","questionnaire","quiz","repeatactivity","resource","subpage","url","CMA","TMA","num_of_prev_attempts","studied_credits","final_result","disability","age_band","imd_band","highest_education","region","gender")

###########################################################
## Convert Categorical String Features to Numeric Values ##
###########################################################

df3$final_result <- as.numeric(factor(df3$final_result , levels=c("Distinction","Fail","Pass","Withdrawn")))
df3$disability <- as.numeric(factor(df3$disability , levels=c("N","Y")))
df3$age_band <- as.numeric(factor(df3$age_band , levels=c("0-35","35-55","55<=")))
df3$imd_band <- as.numeric(factor(df3$imd_band , levels=c( "","0-10%","10-20","20-30%","30-40%" ,"40-50%","50-60%","60-70%","70-80%","80-90%","90-100%")))
df3$highest_education <- as.numeric(factor(df3$highest_education , levels=c("A Level or Equivalent","HE Qualification","Lower Than A Level","No Formal quals","Post Graduate Qualification")))
df3$region <- as.numeric(factor(df3$region , levels=c("East Anglian Region","East Midlands Region","Ireland","London Region","North Region","North Western Region","Scotland","South East Region","South Region","South West Region","Wales","West Midlands Region","Yorkshire Region")))
df3$gender <- as.numeric(factor(df3$gender , levels=c("M","F")))

#################################
## Prepare Outliers in Dataset ##
#################################

source('~/GitHub/Feature-Engineering-Project/Data Upload Assignment/Removing Outliers Script.R')

##################################
## Remove Outliers from Dataset ##
##################################

outlierKD_final_result(df3, final_result)
y
outlierKD2(df3, dataplus)
y
outlierKD3(df3, dualpane)
y
outlierKD4(df3, forumng)
y
outlierKD5(df3, glossary)
y
outlierKD6(df3, homepage)
y
outlierKD7(df3, htmlactivity)
y
outlierKD8(df3, oucollaborate)
y
outlierKD9(df3, oucontent)
y
outlierKD10(df3, ouwiki)
y
outlierKD11(df3, page)
y
outlierKD12(df3, questionnaire)
y
outlierKD13(df3, quiz)
y
outlierKD14(df3, repeatactivity)
y
outlierKD15(df3, resource)
y
outlierKD16(df3, subpage)
y
outlierKD17(df3, url)
y
outlierKD18(df3, CMA)
y
outlierKD19(df3, TMA)
y
outlierKD20(df3, num_of_prev_attempts)
y
outlierKD21(df3, studied_credits)
y

#################################
## Export and Save New Dataset ##
#################################

# Set Project working directory
setwd("~/GitHub/Feature-Engineering-Project/Data Upload Assignment")

write.csv(df3, file = "Normalized_fff_2014j_Dataset.csv")

##########################################
## Export data frame with no N/A Values ##
##########################################

df3_na_none <- na.omit(df3)

write.csv(df3_na_none, file = "Normalized_fff_2014j_Dataset_NA_Removed.csv")
