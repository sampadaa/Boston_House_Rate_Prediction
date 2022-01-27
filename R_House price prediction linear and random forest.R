HousingData <- read.csv("C:/Users/sampa/practice/HousingData.csv")
View(HousingData)


#Exploratory data analysis
any(is.na(HousingData))  #checking for null values
data <- na.omit(HousingData) 
dim(data)
summary(data)

#Description
library(psych)
describe(data)

cor.plot(data)
plot(data)
plot(data[,c(3,5,6,11,13,14)],pch=3)

hist(data$MEDV,xlab="Median Value", main="House Pricing", col="grey")
# the price var is right skewed

#Split into train-test data
library(caret)
set.seed(12345)
sample <- sample.split(data$MEDV,SplitRatio = 0.8)
train <- subset(data,sample == TRUE)
test<-subset(data,sample == FALSE)

#Building the Model
model <- lm(log(MEDV) ~. , data=train) #(MEDV was skewed to the right hence a log transformation would normalize the distribution of MEDV.)
summary (model)
#Adjusted R-squared:  0.7965 

#Predict on test set
pred <- predict(model,test)
results <- cbind(pred,test$MEDV)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
head(results)


#error <- test$MEDV-pred
#rmse <- sqrt(mean(error)^2)

rmse <- sqrt(sum((pred - test$MEDV)^2)/length(test$MEDV))
c(RMSE = rmse, R2 = summary(model)$r.squared)
#      RMSE         R2 
#  20.6714685  0.8049674 


# RANDOM FOREST

#Building the Model
library(randomForest)
model_random <- randomForest(formula = MEDV ~ ., data = train) #(MEDV was skewed to the right hence a log transformation would normalize the distribution of MEDV.)
summary (model_random)

#Predict on test set
pred.rf <- predict(model_random,test)
rmse.rf <- sqrt(sum(((pred.rf) - test$MEDV)^2)/length(test$MEDV))
c(RMSE = rmse.rf, pseudoR2 = mean(model_random$rsq))
#   RMSE  pseudoR2 
# 2.7401505 0.8447453 

plot(pred,test$MEDV, xlab = "Predicted Price", ylab = "Actual Price", pch = 3) #linear
plot(pred.rf,test$MEDV, xlab = "Predicted Price", ylab = "Actual Price", pch = 3) #random forest

#random forest does better as RMSE is reduced significantly and R2 is increased.
