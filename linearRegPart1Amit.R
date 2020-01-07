ls()
rm(list=ls(all=TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Import the libraries#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(caret)
library(corrgram)
library(mice)  # data imputation
library(dummies)  # dummy variable creation


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Import the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

housing.df <- read.csv("D:/dat1/housingdata.csv", sep = ",")
str(housing.df)
head(housing.df)
summary(housing.df)
attach(housing.df)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Conduct exploratory data analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# hist(price)
hist(size)
hist(uds)
# hist(park)
# hist(misc)
# 
# par(mfrow=c(2,2))
# 
# plotInd <- names(housing.df)
# length(plotInd)
# plotInd[2]
# 
# for (i in 1:length(plotInd)) {
#   x <- boxplot(housing.df[,plotInd[i]],main=plotInd[i], horizontal = TRUE)
#   out1 <- x$out
#   print (out1)
#   ind <- which(housing.df[,plotInd[i]] %in% out1)
#   print (plotInd[i])
#   print (ind[i])
#  }
# #  
# # # View(housing.df)
# # 
# # Check the correlation
 library(corrplot)
housing.cor = cor(housing.df)
corrplot(housing.cor)
# 
# housing.df[1,]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     create train and test split (70/30)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(90)  # to reproduce the same result
id <- createDataPartition(housing.df$price, p=0.7,list=FALSE)
id
train <- housing.df[id,]
dim(train)
sum(train$price)

test <- housing.df[-id,]
dim(test)
sum(test$price)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     fit the model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lm(output variable ~ independent variable 1 + independent variable 2 )
set.seed(90) # reproduce the same result
# model 1 

# model 1
fit1 <- lm(price~uds,data=train)
plot(uds,price)
abline(fit1)
summary(fit1)

# model 2 
fit2 <- lm(price~park+misc+uds+size,data=train)
plot(uds,price)
abline(fit2)
summary(fit2)

test$pred <- predict(fit2, newdata = test )

test$err <- (test$price - test$pred)
  
plot(test$price,col='blue', type='l')
lines(test$pred,col='red',type='l')

# Feature Selection
# step(housing.fit, direction=c('backward'))
step(housing.fit, direction=c('both'))

#housing.fit <- lm(price~size+uds+park+misc,data=train)
# cbind(price,housing.fit$fitted.values)
train$pred <- housing.fit$fitted.values
# View(train)
test$pred <- predict(housing.fit, newdata = test )

  
# model 1 RMSE 
SSE = sum(test$pred - test$price)^2
RMSE1 = sqrt(sum(test$pred - test$price)^2)
RMSE1

plot(test$price,col='blue', type='l')
lines(test$pred,col='red',type='l')


# model 2 
fit2 <- lm(price~size+ uds,data=train)
summary(housing.fit2)

# What is R squared ?
# proportion of variation in Y (housig price) is explained by the 
# independent variables

test$pred2 <- predict(housing.fit2, newdata = test )

# x <- cbind(test,test$price - test$pred)
# View(x)


# model 2 RMSE 
# test$pred2<- pred2
SSE = sum(test$pred2 - test$price)^2
RMSE2 = sqrt(sum(test$pred2 - test$price)^2)
RMSE2
# The lesser the RMSE the better the model

plot(test$price,col='blue', type='l')
lines(test$pred2,col='red',type='l')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# validate model assumptions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(car)
# check for linearity
crPlots(fit2)

# Normality of the residuals
qqPlot(fit2)

# check for homoscadasticity
# Ho: there is no heteroscadasticity
# H1 : There exists an heteroscadasticity
library(lmtest)
bptest(fit2)

# check for multicollinearity
# variable inflation factor
vif(fit2)

# Autocorrelation 
# Ho: there is no autocorrelation
# H1 : There exists an autocorrelation
durbinWatsonTest(fit2)

plot(housing.fit2)
