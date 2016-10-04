# Load CSV
df4 <- read.csv("Normalized_fff_2014j_Dataset.csv")

# Look for missing values
apply(df4,2,function(x) sum(is.na(x)))

df4 <- na.omit(df4)

# Select all numeric values and remove columns with large amount of missing values
df5_CMA <- subset(df4, select = -c(final_result, disability, age_band, imd_band, highest_education, region, gender, TMA))

# Split data into CMA_Training and CMA_Test set at 3/4ths split
index <- sample(1:nrow(df5_CMA),round(0.75*nrow(df5_CMA)))
CMA_Train <- df5_CMA[index,]
CMA_Test <- df5_CMA[-index,]

###################################
## Fit a linear regression model ##
###################################

lm.fit <- glm(CMA~., data=CMA_Train)
summary(lm.fit)
pr.lm <- predict(lm.fit,CMA_Test)
MSE.lm <- sum((pr.lm - CMA_Test$CMA)^2)/nrow(CMA_Test)


####################
## Fit Neural Net ##
####################
library(neuralnet)

n <- names(CMA_Train)
f <- as.formula(paste("CMA ~", paste(n[!n %in% "CMA"], collapse = " + ")))
nn <- neuralnet(f,data=CMA_Train,hidden=c(8,8), err.fct="sse",linear.output=TRUE, learningrate = 0.3, threshold = 0.5)

#############################################
## Predicting CMA using the neural network ##
#############################################
CMA_Test1 <- subset(CMA_Test, select = -c(CMA))

CMA_Test <- data.frame(CMA_Test1, CMA_Test$CMA)

names(CMA_Test)[names(CMA_Test) == 'CMA_Test.CMA'] <- 'CMA'

pr.nn <- compute(nn,CMA_Test[,1:19])

pr.nn_ <- pr.nn$net.result*(max(df5_CMA$CMA)-min(df5_CMA$CMA))+min(df5_CMA$CMA)
CMA_Test.r <- (CMA_Test$CMA)*(max(df5_CMA$CMA)-min(df5_CMA$CMA))+min(df5_CMA$CMA)

MSE.nn <- sum((CMA_Test.r - pr.nn_)^2)/nrow(CMA_Test)

##########################
## Compare the two MSEs ##
##########################

print(paste(MSE.lm,MSE.nn))

##########################
## Plot the performance ##
##########################

par(mfrow=c(1,2))

plot(CMA_Test$CMA,pr.nn_,col='red',main='Real vs Predicted Neural Network',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(CMA_Test$CMA,pr.lm,col='blue',main='Real vs predicted Linear Model',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

################
## More Plots ##
################

par(mfrow=c(1,1))

{plot(CMA_Test$CMA,pr.nn_,col='red',main='Real vs Predicted Neural Network',pch=18,cex=0.7)
  points(CMA_Test$CMA,pr.lm,col='blue',pch=18,cex=0.7)
  abline(0,1,lwd=2)
  legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))}

