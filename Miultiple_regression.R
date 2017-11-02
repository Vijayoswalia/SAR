library(MASS)
library(ISLR)

names(Boston)
plot(medv~lstat, Boston)


dataset = read.csv("50_Startups.csv")
dataset
names(dataset)

library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
split
training_set  =subset(dataset, split ==TRUE)
training_set
test_set = subset(dataset, split ==FALSE)
test_set
View(training_set)
View(test_set)


#fitting Multiple linear regression to the training

regressor = lm(formula = Profit ~., data = training_set)

#predicting the test set results
y_pred = predict(regressor, newdata = test_set)
test_set$Profit_Pred = y_pred

#prediction error
test_set$Pred_error = test_set$Profit - test_set$Profit_Pred

#mean absolute deviation
MAD = mean(abs(test_set$Pred_error))
MAD

#mean square error
MSE = mean(test_set$Pred_error^2)
MSE

#root mean square error
RSME = sqrt(mean(test_set$Pred_error^2))
RSME


Pred_Err = function(test_data, prediction_model)
{
  pred = predict(prediction_model, newdata=test_data)
  RMSE = sqrt(mean((test_data$Profit-pred)^2))
  MAD = mean(abs(test_data$Profit-pred))
  return(c(MSE,MAD))
}
Pred_Err(test_data = test_set, regressor)

regressor1 = lm(formula = Profit ~R.D.Spend+Marketing.Spend, data = training_set)
regressor1
regressor = lm(formula = Profit ~R.D.Spend, data = training_set)
summary(regressor)
summary(regressor1)
Pred_Err(test_set, prediction_model = regressor)
Pred_Err(test_set, prediction_model = regressor1)


