####################
## Load Libraries ##
####################

library(tidyr)
library(dplyr)
library(tree)

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

df3 <- df1

# Convert strings to numeric values

df3$final_result <- as.numeric(factor(df3$final_result , levels=c("Distinction","Fail","Pass","Withdrawn")))
df3$disability <- as.numeric(factor(df3$disability , levels=c("N","Y")))
df3$age_band <- as.numeric(factor(df3$age_band , levels=c("0-35","35-55","55<=")))
df3$imd_band <- as.numeric(factor(df3$imd_band , levels=c( "","0-10%","10-20","20-30%","30-40%" ,"40-50%","50-60%","60-70%","70-80%","80-90%","90-100%")))
df3$highest_education <- as.numeric(factor(df3$highest_education , levels=c("A Level or Equivalent","HE Qualification","Lower Than A Level","No Formal quals","Post Graduate Qualification")))
df3$region <- as.numeric(factor(df3$region , levels=c("East Anglian Region","East Midlands Region","Ireland","London Region","North Region","North Western Region","Scotland","South East Region","South Region","South West Region","Wales","West Midlands Region","Yorkshire Region")))
df3$gender <- as.numeric(factor(df3$gender , levels=c("M","F")))


#####################################
## Remove Unwanted columns from DF ##
#####################################

df4 <- subset(df3, select = -c(CMA, TMA))

#####################################################################################
## Split data into final_result_Training and final_result_Test set at 3/4ths split ##
#####################################################################################

index <- sample(1:nrow(df4),round(0.75*nrow(df4)))
final_result_Train <- df4[index,]
final_result_Test <- df4[-index,]

###################################
## Fit a linear regression model ##
###################################

lm.fit <- glm(final_result~., data=final_result_Train)
summary(lm.fit)
pr.lm <- predict(lm.fit,final_result_Test)
MSE.lm <- sum((pr.lm - final_result_Test$final_result)^2)/nrow(final_result_Test)


########################
## Fit Descision Tree ##
########################


n <- names(df4)
frmla <- as.formula(paste("final_result ~", paste(n[!n %in% "final_result"], collapse = " + ")))
tr = tree(frmla, data=df4)
summary(tr)
plot(tr); text(tr)

######################################################
## Predicting final_result using the neural network ##
######################################################

final_result_Test1 <- subset(final_result_Test, select = -c(final_result))

final_result_Test <- data.frame(final_result_Test1, final_result_Test$final_result)

names(final_result_Test)[names(final_result_Test) == 'final_result_Test.final_result'] <- 'final_result'

pr.tr <- predict(tr,final_result_Test[,1:25])

pr.tr_ <- pr.tr["net.result"]*(max(df4$final_result)-min(df4$final_result))+min(df4$final_result)


final_result_Test.r <- (final_result_Test$final_result)*(max(df4$final_result)-min(df4$final_result))+min(df4$final_result)

MSE.tr <- sum((final_result_Test.r - pr.tr_)^2)/nrow(final_result_Test)


print(paste(MSE.lm,MSE.tr))


##########################
## Plot the performance ##
##########################

par(mfrow=c(1,2))

plot(final_result_Test$final_result,pr.tr_,col='red',main='Real vs Predicted Neural Network',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='tr',pch=18,col='red', bty='n')

plot(final_result_Test$final_result,pr.lm,col='blue',main='Real vs predicted Linear Model',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

################
## More Plots ##
################

par(mfrow=c(1,1))

{plot(final_result_Test$final_result,pr.tr_,col='red',main='Real vs Predicted Neural Network',pch=18,cex=0.7)
  points(final_result_Test$final_result,pr.lm,col='blue',pch=18,cex=0.7)
  abline(0,1,lwd=2)
  legend('bottomright',legend=c('tr','LM'),pch=18,col=c('red','blue'))}

