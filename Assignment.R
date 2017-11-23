library(dplyr)
library(caTools)
dataset = read.csv("home_data.csv",stringsAsFactors = F, sep=",")
str(dataset)
head(dataset)
dataset$date = as.Date(substring(dataset$date,1,8),"%Y%m%d")
dataset$date
## Ans 1
agg = aggregate(dataset$price, list(dataset$zipcode), FUN = mean)
agg[agg$x==max(agg$x),]   
(s=filter(agg,agg$x==max(agg$x)))


#Ans 2
(f_sqft = filter(dataset, dataset$sqft_living>2000 & dataset$sqft_living<4000))  
(nrow(f_sqft)/nrow(dataset))*100  

#Ans 3
?subset
basic = data.frame(dataset$bedrooms, dataset$bathrooms, dataset$sqft_living, dataset$sqft_lot, dataset$floors, dataset$zipcode)
str(basic)
basic
split = sample.split(basic$price, SplitRatio = 0.80)
training_set = subset(basic, split==TRUE)
training_set
test_set = subset(basic, split==FALSE)
test_set

fit1 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+zipcode, data=dataset)
summary(fit1)
fit2 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+zipcode, data=dataset)
summary(fit2)
fit3 = lm(price~bedrooms+sqft_living+sqft_lot+zipcode, data=dataset)
summary(fit3)
                                        

regressor = lm(formula = price~., data=training_set)
regressor
y_predict = predict(regressor, newdata = test_set)
test_set$dataset_pred = y_predict
test_diff = test_set$price - test_set$dataset_pred
result_test = data.frame(test_set$price,y_predict, round(test_diff,2))
result_test                                        
