# Load CSV
df4 <- read.csv("Normalized_fff_2014j_Dataset.csv")

# Select all numeric values
df5 <- subset(df3, select = -c(df1.final_result, df1.disability, df1.age_band, df1.imd_band, df1.highest_education, df1.region, df1.gender, CMA))


# Split data into training and test set at 3/4ths split
index <- sample(1:nrow(df5),round(0.75*nrow(df5)))
train <- df5[index,]
test <- df5[-index,]

# Fit a linear regression model and test it on the test set (used for model evalutation later. 
lm.fit <- glm(TMA~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$TMA)^2)/nrow(test)


####################
## Fit Neural Net ##
####################
library(neuralnet)

n <- names(train)
f <- as.formula(paste("TMA ~", paste(n[!n %in% "TMA"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=8, err.fct="sse",linear.output=TRUE, algorithm="backprop", learningrate = 0.3)
