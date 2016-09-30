## Create Neural network from assessment data
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
nn <- neuralnet(score~weight+submission_time+CMA+TMA, data = train, hidden = 1, linear.output = TRUE)
