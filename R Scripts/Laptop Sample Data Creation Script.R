## Laptop Test Script ##

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

###################################
#### Begin Data Pre-Processing ####
###################################

# Create new data frame by combining Assessment data with Student Assessment data
combined_assessment <- merge(std_assessments, assessments, by = "id_assessment", all = TRUE)

# Create assessment data from course FFF
FFF_Assessment <- subset(combined_assessment, code_module %in% "FFF")

# Select one semester of the course
df_2014j <- subset(FFF_Assessment, code_presentation %in% "2014J")

# Select student ID to test
student_101217 <- subset(df_2014j, id_student %in% "101217")
student_552173 <- subset(df_2014j, id_student %in% "552173")
student_655386 <- subset(df_2014j, id_student %in% "655386")

# Merge sample student data together
student_test_data <- rbind(student_655386,student_552173,student_101217)

# Create new data frame by combining Assessment data with Student VLE data
combined_vle_assess <- merge(student_test_data, std_vle, by= c("id_student", "code_module","code_presentation"))

# Create new data frame by student vle information data with vle discription data
combined_std_vle_assess <- merge(combined_vle_assess, vle, by= c("id_site", "code_module","code_presentation"))

# Create new data frame by student profile information data and overall student information discription data
sample_student_data <- merge(combined_std_vle_assess, std_info, by= c("id_student", "code_module","code_presentation"))

# Remove uneeded columns from dataframe
sample_student_data <- subset(sample_student_data, select = -c(id_site, code_module, code_presentation) )

