# Load CSV
df4 <- read.csv("Normalized_fff_2014j_Dataset.csv")

# Select all numeric values
df5 <- subset(df4, select = -c(df1.final_result, df1.disability, df1.age_band, df1.imd_band, df1.highest_education, df1.region, df1.gender, CMA))

# Remove Outliers
source('~/GitHub/Feature-Engineering-Project/R Scripts/Remove outliers from dataset.R')
source('~/GitHub/Feature-Engineering-Project/R Scripts/2.R')
source('~/GitHub/Feature-Engineering-Project/R Scripts/3.R')

outlierKD1(df5, TMA)
outlierKD2(df5, dataplus)
outlierKD3(df5, dualpane)

# Split data into training and test set at 3/4ths split
index <- sample(1:nrow(df5),round(0.75*nrow(df5)))
train <- df5[index,]
test <- df5[-index,]

###################################
## Fit a linear regression model ##
###################################

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
nn <- neuralnet(f,data=train,hidden=c(8,8), err.fct="sse",linear.output=TRUE, learningrate = 0.3, threshold = 0.5)

#############################################
## Predicting TMA using the neural network ##
#############################################
test1 <- subset(test, select = -c(TMA))

test <- data.frame(test1, test$TMA)

names(test)[names(test) == 'test.TMA'] <- 'TMA'

pr.nn <- compute(nn,test[,1:20])

pr.nn_ <- pr.nn$net.result*(max(df5$TMA)-min(df5$TMA))+min(df5$TMA)
test.r <- (test$TMA)*(max(df5$TMA)-min(df5$TMA))+min(df5$TMA)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)

##########################
## Compare the two MSEs ##
##########################

print(paste(MSE.lm,MSE.nn))

##########################
## Plot the performance ##
##########################

par(mfrow=c(1,2))

<<<<<<< HEAD
plot(test$TMA,pr.nn_,col='red',main='Real vs predicted Neural Network',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n', cex=.95)

plot(test$TMA,pr.lm,col='blue',main='Real vs Predicted Linear Model',pch=18, cex=0.7)
=======
{plot(test$TMA,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)}

{plot(test$TMA,pr.lm,col='blue',main='Real vs predicted LM',pch=18, cex=0.7)
>>>>>>> origin/master
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)}

################
## More Plots ##
################

plot(test$TMA,pr.nn_,col='red',main='Real vs Predicted Neural Network',pch=18,cex=0.7)
points(test$TMA,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

