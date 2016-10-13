## Extracting exam scores

library(plyr)

assessment_exam_df1 <- ddply(std_assessments_df, .(id_student), function(x) x[nrow(x), ])
