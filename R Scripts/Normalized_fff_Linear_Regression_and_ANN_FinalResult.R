# Load CSV
df4 <- read.csv("Normalized_fff_Dataset_NA_Removed.csv")

# Look for missing values
apply(df4,2,function(x) sum(is.na(x)))

df4 <- na.omit(df4)

# Select all numeric values and remove columns with large amount of missing values
df5_final_result <- subset(df4, select = -c(id_student, TMA, CMA))

# Remove Final Result Column
df5_final_result <- subset(df5_final_result, select = -c(X))

# Rename Columns

names(df5_final_result) <- c("dataplus","dualpane","folder","forumng","glossary","homepage","htmlactivity","oucollaborate","oucontent","ouelluminate","ouwiki","page","questionnaire","quiz","repeatactivity","resource","subpage","url","num_of_prev_attempts","studied_credits","final_result","disability", "age_band","imd_band", "highest_education", "region", "gender")

# Split data into final_result_Training and final_result_Test set at 3/4ths split
index <- sample(1:nrow(df5_final_result),round(0.75*nrow(df5_final_result)))
final_result_Train <- df5_final_result[index,]
final_result_Test <- df5_final_result[-index,]

###################################
## Fit a linear regression model ##
###################################

lm.fit <- glm(final_result~., data=final_result_Train)
summary(lm.fit)
pr.lm <- predict(lm.fit,final_result_Test)
MSE.lm <- sum((pr.lm - final_result_Test$final_result)^2)/nrow(final_result_Test)


####################
## Fit Neural Net ##
####################
library(neuralnet)

n <- names(final_result_Train)
f <- as.formula(paste("final_result ~", paste(n[!n %in% "final_result"], collapse = " + ")))
nn <- neuralnet(f,data=final_result_Train,hidden=c(10,10), err.fct="sse",linear.output=TRUE, learningrate = 0.3, threshold = 0.5)

#############################################
## Predicting final_result using the neural network ##
#############################################
final_result_Test1 <- subset(final_result_Test, select = -c(final_result))

final_result_Test <- data.frame(final_result_Test1, final_result_Test$final_result)

names(final_result_Test)[names(final_result_Test) == 'final_result_Test.final_result'] <- 'final_result'

pr.nn <- compute(nn,final_result_Test[,1:26])

pr.nn_ <- pr.nn$net.result*(max(df5_final_result$final_result)-min(df5_final_result$final_result))+min(df5_final_result$final_result)
final_result_Test.r <- (final_result_Test$final_result)*(max(df5_final_result$final_result)-min(df5_final_result$final_result))+min(df5_final_result$final_result)

MSE.nn <- sum((final_result_Test.r - pr.nn_)^2)/nrow(final_result_Test)

##########################
## Compare the two MSEs ##
##########################

print(paste(MSE.lm,MSE.nn))

##########################
## Plot the performance ##
##########################

par(mfrow=c(1,2))

plot(final_result_Test$final_result,pr.nn_,col='red',main='Real vs Predicted Neural Network',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(final_result_Test$final_result,pr.lm,col='blue',main='Real vs predicted Linear Model',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

################
## More Plots ##
################

par(mfrow=c(1,1))

{plot(final_result_Test$final_result,pr.nn_,col='red',main='Real vs Predicted Neural Network',pch=18,cex=0.7)
  points(final_result_Test$final_result,pr.lm,col='blue',pch=18,cex=0.7)
  abline(0,1,lwd=2)
  legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))}

