# Load Libraries
library(tidyr)

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

