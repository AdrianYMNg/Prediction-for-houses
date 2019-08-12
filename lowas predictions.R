library(statsr)
library(dplyr)
library(BAS)
library(corrplot)
library(GGally)
library(ggplot2)
library(MASS)
library(class)
library(caret)
library(e1071)

setwd("C:\\Users\\Admin\\Documents\\Project1")#------ change when upload to cloud 
source("Load connection.R")

#Splitting data into 2 sets
rows <- nrow(database)
splitrow <- (nrow(database))/2
roundsplit <- round((as.numeric(splitrow)))
roundplus <- roundsplit + 1
my_data <- database[1:roundsplit,]
my_test <- database[roundplus:rows,]

#Find out column with missing data to not calculate them
k <- colSums(is.na(my_data))
missingval<-sort(k, decreasing = TRUE)[1:20]
barplot(missingval, main = "Missing values", las = 2 )
hist(my_data$SalePrice, main = " House price", xlab = "price", ylab = "amount", col = "green")

#Find out what affects the prices
summary(my_data$SalePrice)
hist(log(my_data$LotArea), main = "Log:Area", xlab = "area", col = "red")
hist(my_data$YearBuilt, main = "Year of Building", col = "blue")
boxplot(my_data$SalePrice~my_data$OverallCond, main = "Overall Condition", xlab = "condition", col = "yellow")
hist(my_data$OverallCond)

overallcond <- my_data %>% filter(OverallCond != 5)%>% group_by(OverallCond)%>% summarise(median = median(SalePrice), mean = mean(SalePrice))
table(my_data$OverallCond)
plot(overallcond, col = "red")

#Finding the graphs of correlation
plot(my_data$SalePrice~my_data$LotArea, main = "Area vs Price", xlab = "area", col = "red")
plot(log(my_data$SalePrice)~log(my_data$LotArea), main = "Log:Area vs Price", xlab = "area", col = "red")
plot(my_data$SalePrice~my_data$YearBuilt, main = "Year of Building vs Price", col = "blue")
plot(my_data$SalePrice~my_data$OverallCond, main = "Overall Condition vs Price", xlab = "condition", col = "yellow")

#Create model based on dicision of which column is impotant from the plots 
model.lm=lm(log(SalePrice) ~  OverallCond + log(LotArea) + YearBuilt + GarageArea + TotalBsmtSF + GarageCars +  
              FullBath + HalfBath + BedroomAbvGr +  X1stFlrSF + X2ndFlrSF + log(LotArea) +  CentralAir , data=my_data )
summary(model.lm)

model.bic.full=bas.lm(log(SalePrice) ~  OverallCond+ log(LotArea)+ YearBuilt+ TotalBsmtSF + GarageCars + 
                        BedroomAbvGr + log(LotArea) +  CentralAir, prior = "BIC", method = "MCMC",modelprior = uniform(),data=my_data)
summary(model.bic.full)

lm.bic = lm(log(SalePrice) ~  OverallCond+ log(LotArea)+ YearBuilt+ TotalBsmtSF + GarageCars + 
              BedroomAbvGr + log(LotArea) +  CentralAir,data=my_data)

# Extract Predictions for bic fro training data
predict.full.train.bic <- exp(predict(lm.bic, my_data))
resid.full.train.bic <- my_data$SalePrice - predict.full.train.bic
rmse.full.train.bic <- sqrt(mean(resid.full.train.bic^2, na.rm = TRUE))
rmse.full.train.bic

# Extract Predictions for test data
predict.full.test <- exp(predict(lm.bic, my_test))
resid.full.test <- my_test$SalePrice - predict.full.test
rmse.full.test.bic <- sqrt(mean(resid.full.test^2, na.rm = TRUE))
rmse.full.test.bic
rmse.full.train.bic < rmse.full.test.bic


#Refining model by filtering out data
allvar.model.step1 = lm(SalePrice ~ LotArea + OverallCond + YearBuilt + YearRemodAdd + Heating + CentralAir +  BedroomAbvGr+ KitchenAbvGr +
                           KitchenQual  + TotRmsAbvGrd+ Fireplaces + GarageYrBlt + GarageArea + PoolArea , data = my_data)
summary(allvar.model.step1)
train_normal <- my_data%>% filter(SaleCondition == "Normal" )%>%
   dplyr::select(SalePrice, LotArea, OverallCond, YearBuilt, YearRemodAdd, Heating, CentralAir, BedroomAbvGr, KitchenAbvGr, KitchenQual, TotRmsAbvGrd,Fireplaces, GarageYrBlt,GarageArea, PoolArea)

allvar.model.step2 = lm(log(SalePrice) ~ log(LotArea)  + OverallCond + YearBuilt + YearRemodAdd + Heating + CentralAir +  BedroomAbvGr+ KitchenAbvGr +  KitchenQual  + TotRmsAbvGrd+ Fireplaces + GarageYrBlt + log(GarageArea) + PoolArea , data = train_normal)
summary(allvar.model.step2)

train_normal_2 <- my_data%>%
  filter(SaleCondition == "Normal")%>% dplyr::select(SalePrice, LotArea, OverallCond, YearBuilt, YearRemodAdd, Heating, CentralAir, BedroomAbvGr, KitchenAbvGr, KitchenQual, Fireplaces, GarageArea) 

allvar.model.step3 = lm(log(SalePrice) ~ ., data = train_normal_2)
summary(allvar.model.step3)

train_normal_3 <- my_data%>%  filter(SaleCondition == "Normal" )%>%
  dplyr::select(SalePrice, LotArea, OverallCond, YearBuilt, CentralAir, KitchenQual, Fireplaces) 

allvar.model.step4 = lm(log(SalePrice) ~ log(LotArea)+ OverallCond+ YearBuilt+CentralAir+ KitchenQual+ Fireplaces, data = train_normal_3)
summary(allvar.model.step4)

model.step4.bas = bas.lm(log(SalePrice) ~ log(LotArea)+ OverallCond+ YearBuilt+CentralAir+ KitchenQual+ Fireplaces, prior = "BIC",
                         method = "MCMC",
                         modelprior = uniform(), data = train_normal_3)
summary(model.step4.bas)

final.model=lm(log(SalePrice) ~ log(LotArea)+ OverallCond+ YearBuilt+CentralAir+ KitchenQual+ Fireplaces, data = train_normal_3)

model.train<-mean(exp(predict(final.model, my_data)))
model.test<-mean(exp(predict(final.model, my_test)))
model.train>model.test


# Predict prices for First set data
predict.train <- exp(predict(final.model, my_data, interval = "prediction"))
coverage.prob.train <- mean(my_data$SalePrice > predict.train[,"lwr"] &
                              my_data$SalePrice < predict.train[,"upr"])
coverage.prob.train

# Predict prices for Second set of data
predict.full <- exp(predict(final.model, my_test, interval = "prediction"))
coverage.prob.full <- mean(my_test$SalePrice > predict.full[,"lwr"] &
                             my_test$SalePrice < predict.full[,"upr"])
coverage.prob.full

#Getting accuracy by averaging the predicted prices
accuracy <- function()
{
  accuracy <- (coverage.prob.train+coverage.prob.full)/2
  print(paste("Accuracy is ", accuracy))
}
accuracy()
bind_data <- rbind(train_normal_3,input_data)
outpredict <- nrow(bind_data)
GetPrediction <- function()
{
  bind_data <- rbind(train_normal_3,input_data)
  pred <- predict(final.model,bind_data[outpredict,])
  Result <- exp(pred)
  print(paste("Prediction Result is ", Result))
  return(Result)
}
GetPrediction()
Result = GetPrediction()

