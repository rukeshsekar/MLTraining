print ('Om Gam Ganapataye Namah')
print ('Om Gurave Namah')
print('Om Swami malai muruga potri')
print('Om hayagreeva Potri')

#clean workspace
rm(list = ls()) 

setwd("D:/dat1")
a <- read.csv("D:/dat1/housingdata.csv", sep = ",")
# There are times when a standard regression line just doesn't
# capture the relationship. In this case, the loess() function in R 
# (also lowess()) will fit a non-linear line to the data. Here we've 
# drawn the loess curve in blue
str(a)
head(a)
colnames(a)
a$price <- as.integer(gsub(',','',a$price))
head(a[a$price <= 10,c(3,4)])
a$size <- as.integer(a$X12size)
str(a)
a$X12size <- NULL
a$storeRoom <- NULL
a$CustName <- NULL

set.seed(90)
ind <- sample(1:nrow(a),round(nrow(a) * 0.75))
ind
train <- a[ind,]
test <- a[-ind,]

dim(train)
dim(test)
# 112+37
# lm(dependent variable ~ independent variable , data=train)
lfit <- lm(price~size,data=train)
summary(lfit)
pred <- predict(lfit,newdata=test)

confint(lfit, level=0.95)
lfit1 <- lm(price~size+uds+park+misc,data=train)
summary(lfit1)
pred <- predict(lfit1,newdata=test)



attributes(a)
class(a)
typeof(a)
dim(a)
var(a$price)
range(a$price)

a['price']
a[1:10,]
a[,3:4]
"12345" == 12345

"12345" + 2 == 12345 + 2
"5" == 5
as.numeric("5") == 5
"3" == 5


pairs(a)

#------------------Split data into Train and Test --------------------------
# ---------feature selection -------------------
# ---------------------------------------------------------------------
str(a)
library(caret)
library(randomForest)
set.seed(123)
control <- rfeControl(functions=rfFuncs,method = "cv",number = 10)
rfe.df <- rfe(a[,1:3], a[,4],sizes=1:12,rfeControl=control)
print(rfe.df)
plot(rfe.df, type=c("g", "o"), cex = 1.0, col = 1:11)
predictors(rfe.df)

# --------------------------------------------------------------

# install.packages("Boruta")
library(Boruta)

# set.seed(123)
boruta.train <- Boruta(a$price~., data = a, doTrace = 2)
print(boruta.train)

# Removing Na's
# a <- read.csv("HousingS.csv",header=T)
# a <- na.omit(a)
# str(a)


# set.seed(300)
# ind <-sample(1:nrow(a),round(nrow(a)*.75))  
# tr <- a[ind,]
# te <- a[-ind,]
# rm(ind)
# plot(tr$price~tr$size)
# str(tr)
# head(tr)


lfit <- lm(price~size,data=tr11)

#  plot(tr$price~tr$uds)
#  plot(tr$price~tr$park)
# 
# # par(mar = rep(2,4))
# par(mfrow = c(2,2) )
# par(mfrow = c(1,1))
# 

# ----------  Exploratory Analysis ------------------------------
library(corrgram)
corrgram(tr[,1:4],order=TRUE)
# Observation : there is a slight correlation between park and uds
pairs(tr[,1:5])
# ----------  Identify Outliers & Treat  ------------------------------
# str(tr)
boxplot(tr[,2:4],horizontal = TRUE)
# Observation : there is an outlier for park


str(tr)
boxplot(park,horizontal = TRUE)

fix.outlier <- function(df, feat){
  if(is.numeric(df[,feat])){
    per95 <- quantile(df[,feat],0.95)
    ifelse(df[,feat]> per95,per95,df[,feat])
  }
}

# str(tr)
# quantile(tr[,3],0.95)
# summary(tr$park)

tr$park <- fix.outlier(tr,3)

quantile(tr$park)

summary(tr)
summary(tr$size)
hist(tr$park)
hist(tr$size)
hist(tr$uds)

quantile(tr$park,0.95)
attach(tr)
# t <- read.csv("D:/dat1/fbook.csv")
# quantile(t$park)

# Null hypothesis :  Housing price of "size", "uds" or "park" 
# do not have an effect on price

# Feature selection 
# f <- lm(tr$price~.,data=tr)
# step(f,direction="both")

# What is intercept
size=0
price1=1000
tr1=data.frame(price1,size)
lfit1 <- lm(price1~size,data=tr1)
lfit1
# in other words y doesnot depend on x

# What happens when slope is 0 ?


# What happens when u r intercept is 0 ?
# if u r intercept is 0 , u hv a straight line that now passes through the 0,0 points
# in otherwords there is no intercept.
# u will have the line with the same slope but it pass through te 
size1=c(0)
size1=c(100,120,140,160,190)
price2=c(1000,2000,2600,2800,3000)
# intercept formula 
# mean(price2) - 50 * mean(size1)
plot(size1,price2)

tr11=data.frame(price2,size1)
lfit11 <- lm(price2~size1,data=tr11)
lfit11
lfit11$coef[1] + lfit11$coef[2] * 100  # y = bo + b1x
# This equation estimates that for each increase of 100 square foot, 
# the expected selling price are predicted to increase by 1000
lfit11$coef[2]
lfit11$fitted.values
lfit11$effects


summary(lfit11)
lfit11$model
anova(lfit11$terms)

# Y = bo + b1x + e 
# y is the target and x is the inrfluencing or indep variable
# e is simply random variation , the error , 




# indep variables 

# coefficients: Betas
# minimizes : least
# Sum of squared differences : Square of residuals
# Estimated line : Regression line
# Actual Values : Values in data set


# Model 1  
lfit1 <- lm(tr$price~size,data=tr)
summary(lfit1)

lmtest::bptest(lfit1)

bcsize <- caret::BoxCoxTrans(tr$size)
summary(tr$size)
summary(bcsize)
tr$size <- cbind(tr$size,bcsize1 = predict(bcsize,tr$size))

lfit11 <- lm(tr$price~tr$size,data=tr)
summary(lfit11)

pred11 <- predict(lfit11,newdata = te)

lmtest::bptest(lfit11)

library(sandwich)
vcovHC(lfit1)
waldtest(lfit1,vcov=vcovHC())




lfit1$model
lfit1$fitted.values
anova(lfit1)
abline(lfit1)
resid(lfit1)
shapiro.test(resid(lfit1))

# par(mfrow = c(2,2) )
# y = b0 + b1x 
# plot(lfit2)
pred1 <- predict(lfit1,newdata=te)
predict(lfit1,data.frame(size=2200))
lfit1$coef[1]+ lfit1$coef[2] * 2200

residuals(lfit1)
coef(lfit1)
coef(summary(lfit1))

#    hist(lfit2$residuals)
plot(tr$price~tr$size)
abline(lfit1)

plot(te$price,col="blue",type="l")
lines(pred1,col="red",type="l")

actual <-te$price
pred <- pred1


EvalModel <- function(x,y) {
  SSE=sum((pred-actual)^2)
  SST = sum((mean(actual) - actual)^2)
  rmse = round(sqrt(mean(actual- pred)^2),3)
  Rsquared = 1-SSE/SST
  print(paste("SSE =",SSE,"SST=",SST,"Validation Set RSquare = ",Rsquared,"RMSE=",rmse))
}

EvalModel(x,y)


plot(te$price,col="blue",type="l")
lines(pred,col="red",type="l")

cbind(te$price,pred1)


# Normality of the Residual
layout(matrix(1:2, ncol = 2))
plot(lfit1, which = 2)
car::qqPlot(residuals(lfit1), id.n = 5)
# Look for the tails, points should be close to the line or within the confidence intervals.
# Quantile plots compare the Studentized residuals vs a t-distribution
# Other tests: shapiro.test(), mshapiro.test() in library(mvnormtest)-library(ts)

# Homoscedasticity


library(rms)
robcov(lfit1)

layout(matrix(1:2, ncol = 2))
plot(lfit1, which = 1)
## Raw residual
car::residualPlots(lfit1, terms = ~ 1, fitted = T, id.n = 5, smoother = loessLine)

# Spread level plot basically does the same thing. Non-constant variance test tests for it.
car::spreadLevelPlot(lfit1, id.n = 5)

# When there are strong linear relationships among the predictors in a
# regression analysis, the precision of the estimated regression coefficients 
# in linear models declines compared to what it would have been were the
# predictors uncorrelated with each other

# H0: 
# Model 2
lfit2 <- lm(tr$price~size+uds,data=train)
summary(lfit2)
par(mfrow=c(2,2))
plot(lfit2)

lmtest::bptest(lfit2)
# H0: Variance is constant
# Ha: Variance is not constant 

# we could see that p value is not less than alpha , hence we fail to reject the H0



  
aov(tr$price~size+uds,data=tr)
# par(mfrow = c(2,2) )
# plot(lfit2)
pred2 <- predict(lfit2,newdata=te)

reslfit2 <- resid(lfit2)

qqnorm(reslfit2, ylab="Standardized Residuals", xlab="Normal values")



x <- log(tr$size)

# Multicollinearity demo
mcol <- lm(tr$price~size+uds+park,data=tr)
summary(mcol)

mcol <- lm(tr$price~size+uds+park+misc,data=tr)

library(car)
vif(mcol)

step(mcol,direction="backward")

library(caret)

# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))

# train the model
model <- train(tr$price~., data=tr, trControl=train_control, method="lm", tuneGrid=grid)
model <- train(x = tr$size+tr$uds , y = tr$price, trControl=train_control, method="lm")
warnings()

# summarize results
print(model)


#  RMSE is the square root of the mean of the squared errors 
EvalModel <- function(Actual,Pred) {
  SSE=sum((Pred-Actual)^2)
  SST = sum((mean(Actual) - Actual)^2)
  rmse = round(sqrt(mean(Actual- Pred)^2),3)
  Rsquared = 1-SSE/SST
  print(paste("SSE =",SSE,"SST=",SST,"Validation Set RSquare = ",Rsquared,"RMSE=",rmse))
}
x <-te$price
y <- pred2

EvalModel(x,y)
vAssump(lfit2)

plot(te$price,col="blue",type="l")
lines(pred2,col="red",type="l")

library(lmtest)
# Violation of homoscedasticity (Yes if Breusch-Pagan test's p < 0.05)
bp <- bptest(lfit1)

### Multi-collinearity
library(car)
vif(lfit2)
# must not be
if (length(fit$coefficients)<3) 
{ vf=NA; sq.vf=NA 
else 
  
{vf=round(vif(lfit2),3);          # variance inflation factors
 sq.vf <- sqrt(vif(lfit2)) > 2}    # problem?

### Non-independence of Errors
### Test for Autocorrelated Errors
NI <- "Durbin-Watson test for non-independence of Errors (DW statistics should be near 2, with p > 0.05)"
DW <- durbinWatsonTest(lfit2)


plot(te$price,col="blue",type="l")
lines(pred2,col="red",type="l")


tr$park <- fix.outlier(tr,3)

# Model 3  
lfit3 <- lm(tr$price~size+uds+park,data=tr)
summary(lfit3)

lmtest::bptest(lfit3)

# par(mfrow = c(2,2) )
# plot(lfit2)
pred3 <- predict(lfit3,newdata=te)
# hist(lfit2$residuals)
x <-te$price
y <- pred3

EvalModel(x,y)


plot(te$price,col="blue",type="l")
lines(pred3,col="red",type="l")

# Obtain the Sum ofo Squared Errors (SSE) on the test set
SSE = sum((pred2 - te$price)^2)

# Obtain the Total Sum of Squares (SST) on the test set
SST = sum((mean(te$price) - te$price)^2)

# Obtain the R-squared on the test set
Rsquared = 1-SSE/SST

print(paste("SSE=", SSE, "SST=", SST, "Rsquared=", Rsquared))


# Model 4  
hist(log(tr$park))
lfit4 <- lm(tr$price~size+uds+bcpark,data=tr)
summary(lfit4)

bcpark <- caret::BoxCoxTrans(tr$park)
summary(tr$park)

# par(mfrow = c(2,2) )
# plot(lfit2)
pred4 <- predict(lfit4,newdata=te)
# hist(lfit2$residuals)
x <-te$price
y <- pred4

EvalModel(x,y)

# cbind(park,log(park),exp(log(park)))

plot(te$price,col="blue",type="l")
lines(pred4,col="red",type="l")

# # Model 3
# lfit3 <- lm(tr$price~size+uds+park,data=tr)
# summary(lfit3)
# 
# # Model 4 - Excluding intercept
# lfit4 <- lm(tr$price~-1+size+uds,data=tr)
# summary(lfit4)

# library(car)
# vif(lfit2)
# vif(lfit3)


#------------------Perfrom Cross Validation  --------------------------
# this is the way you can estimate the out of sample error 
# the goal here is to avoid overfitting is one of the key issues of the
# predicted functions
# you want to your data you're predicted function too closley to 
# the data set you have hand
# The other goal is to make your predictions generalize well or
# to compare different potential
# models and pick the one that you think will work best on the new data set
# --------------------  K fold  ------------------------
library(DAAG)  
set.seed(500)
val_diag <- CVlm(data= tr, m=5,form.lm=formula(price~size+uds))
print(val_diag)
attributes(val_diag)$ms

library(DAAG)
cvResults <- suppressWarnings(CVlm(data=tr, form.lm=price ~ size+uds, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error

# --------------------  LOOCV  ------------------------
library(ISLR)
library(boot)

model = glm(price~size,data=tr)
loocv_mse <- cv.glm(tr,model)$delta[1]
loocv_mse
loocv_mse = NULL

for (i in 1:10) {
  model = glm(price~size+uds,data=tr)
  loocv_mse[i] = cv.glm(tr,model)$delta[1]
}
loocv_mse

# -------------------------------------------------------

library(caret)
# load the iris dataset
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb", tuneGrid=grid)
# summarize results
print(model)

? lead
v <- 1:16
v
v <- v[16:1]

v[16:1] <- v[16:1]
V[1,16 ] <- V[16:1]



food <- read.table("http://www.stat.sc.edu/~hitchcock/foodstuffs.txt", header=T)

attach(food)

# The hclust function requires that a distance object be input:

# Let's first scale the data by dividing each variable by its standard deviation:

std <- apply(food[,-1], 2, sd) # finding standard deviations of variables
food.std <- sweep(food[,-1],2,std,FUN="/") 

# Calculating pairwise Euclidean distances between the (standardized) objects:

dist.food <- dist(food.std)

# Single linkage:

food.single.link <- hclust(dist.food, method='single')

# Plotting the single linkage dendrogram:

plclust(food.single.link, labels=Food, ylab="Distance")

windows() # opening new window while keeping previous one open

# complete linkage:

food.complete.link <- hclust(dist.food, method='complete')

# Plotting the complete linkage dendrogram:

plclust(food.complete.link, labels=Food, ylab="Distance")

windows() # opening new window while keeping previous one open

# Average linkage:

food.avg.link <- hclust(dist.food, method='average')

# Plotting the average linkage dendrogram:

plclust(food.avg.link, labels=Food, ylab="Distance")








# Note the complete linkage algorithm is slightly less prone to forming 
# "outlier-only" clusters here.

# Cutting the complete-linkage dendrogram to form k=2 clusters here:

cut.2 <- cutree(food.complete.link, k=2)
cut.2     # printing the "clustering vector"

food.2.clust <- lapply(1:2, function(nc) Food[cut.2==nc])  
food.2.clust   # printing the clusters in terms of the Food labels

# Suppose we preferred a 5-cluster solution:

cut.5 <- cutree(food.complete.link, k=5)

# Equivalently, in this case:
cut.5 <- cutree(food.complete.link, h=3.5)  
# h specifies the height at which the dendrogram should be cut

cut.5   # printing the "clustering vector"

food.5.clust <- lapply(1:5, function(nc) Food[cut.5==nc])  
food.5.clust   # printing the clusters in terms of the Food labels



############# Visualization of Clusters:

### Via the scatterplot matrix:

pairs(food[,-1], panel=function(x,y) text(x,y,cut.5))

# Cluster 1 seems to be the high-fat, high-energy foods (beef, ham, pork)
# Cluster 2 foods seem to have low iron (more white meats than red meats)
# Cluster 4 foods have low protein (the clams)
# Cluster 5 is a high-calcium outlier (canned sardines)

### Via a plot of the scores on the first 2 principal components, 
### with the clusters separated by color:

food.pc <- princomp(food[,-1],cor=T)

# Setting up the colors for the 5 clusters on the plot:
my.color.vector <- rep("green", times=nrow(food))
my.color.vector[cut.5==2] <- "blue"
my.color.vector[cut.5==3] <- "red"
my.color.vector[cut.5==4] <- "orange"
my.color.vector[cut.5==5] <- "brown"

# Plotting the PC scores:

par(pty="s")
plot(food.pc$scores[,1], food.pc$scores[,2], ylim=range(food.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(food.pc$scores[,1], food.pc$scores[,2], labels=Food, cex=0.7, lwd=2,
     col=my.color.vector)

# What would this plot look like for the 2-cluster solution?
# For the 3-cluster solution?












# The mtcars data set is built into R:

help(mtcars)

# We will focus on the variables that are continuous in nature rather than discrete:

cars.data <- mtcars[,c(1,3,4,5,6,7)]

# Standardizing by dividing through by the sample range of each variable

samp.range <- function(x){
  myrange <- diff(range(x))
  return(myrange)
}
my.ranges <- apply(cars.data,2,samp.range)
cars.std <- sweep(cars.data,2,my.ranges,FUN="/") 


# Getting distance matrix:

dist.cars <- dist(cars.std)

# Single linkage:

cars.single.link <- hclust(dist.cars, method='single')

# Plotting the single linkage dendrogram:

plclust(cars.single.link, labels=row.names(cars.data), ylab="Distance")

windows() # opening new window while keeping previous one open

# complete linkage:

cars.complete.link <- hclust(dist.cars, method='complete')

# Plotting the complete linkage dendrogram:

plclust(cars.complete.link, labels=row.names(cars.data), ylab="Distance")

windows() # opening new window while keeping previous one open

# Average linkage:

cars.avg.link <- hclust(dist.cars, method='average')

# Plotting the average linkage dendrogram:

plclust(cars.avg.link, labels=row.names(cars.data), ylab="Distance")


# Average Linkage dendrogram seems to indicate two major clusters, 
# Single Linkage dendrogram may indicate three.

# Single Linkage Solution:

cut.3 <- cutree(cars.single.link, k=3)
cut.3     # printing the "clustering vector"

cars.3.clust <- lapply(1:3, function(nc) row.names(cars.data)[cut.3==nc])  
cars.3.clust   # printing the clusters in terms of the car names


# Cluster 1 seems to be mostly compact cars, Cluster 2 is sports cars, Cluster 3 is large luxury sedans


############# Visualization of Clusters:

### Via the scatterplot matrix:

pairs(cars.data, panel=function(x,y) text(x,y,cut.3))

# Cluster 1 cars tend to have high mileage, low displacement, low horsepower, low weight.
# Cluster 3 cars tend to have low mileage, high weight.

### Via a plot of the scores on the first 2 principal components, 
### with the clusters separated by color:

cars.pc <- princomp(cars.data,cor=T)

carnames <- abbreviate(row.names(cars.data))

# Setting up the colors for the 3 clusters on the plot:
my.color.vector <- rep("green", times=nrow(cars.data))
my.color.vector[cut.3==2] <- "blue"
my.color.vector[cut.3==3] <- "red"

# Plotting the PC scores:

par(pty="s")
plot(cars.pc$scores[,1], cars.pc$scores[,2], ylim=range(cars.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(cars.pc$scores[,1], cars.pc$scores[,2], labels=carnames, cex=0.7, lwd=2,
     col=my.color.vector)

# It's clear that PC1 is more important in separating the clusters than PC2.










#####################    Ward's method:  #####################################
##
## To perform Ward's method of hierarchical clustering, alter the above code to:
##
##         cars.ward <- hclust(dist.cars, method='ward')
##
## and proceed as with the linkage methods.
##
###############################################################################



#################################################################
#################################################################
###########
###########           Partitioning Clustering        
###########
#################################################################
#################################################################



##########################
##
##  K-means clustering
##
##########################

###########################################
###  Foodstuffs example 
###########################################

# Consider the food.std data frame given above.

# A K-means clustering with k = 5:

# Note that the stability of the result can be improved by increasing the maximum number 
# of iterations and using multiple random starts:

food.k5 <- kmeans(food.std, centers=5, iter.max=100, nstart=25)
food.k5

# Let's try k=4:

food.k4 <- kmeans(food.std, centers=4, iter.max=100, nstart=25)
food.k4

# Printing the clustering vector for the 4-cluster solution:

food.k4$cluster

food.k4.clust <- lapply(1:4, function(nc) Food[food.k4$cluster==nc])  
food.k4.clust   # printing the clusters in terms of the Food labels







############# Visualization of Clusters:

### Via the scatterplot matrix:

pairs(food[,-1], panel=function(x,y) text(x,y,food.k4$cluster))

# Cluster 1 foods tend to be high in calcium. (this comment does not reflect all runs of the algorithm)
# Cluster 4 foods tend to be high in fat. (this comment does not reflect all runs of the algorithm)


### Via a plot of the scores on the first 2 principal components, 
### with the clusters separated by color:

food.pc <- princomp(food[,-1],cor=T)

# Setting up the colors for the 5 clusters on the plot:
my.color.vector <- rep("green", times=nrow(food))
my.color.vector[food.k4$cluster==2] <- "blue"
my.color.vector[food.k4$cluster==3] <- "red"
my.color.vector[food.k4$cluster==4] <- "orange"

# Plotting the PC scores:

par(pty="s")
plot(food.pc$scores[,1], food.pc$scores[,2], ylim=range(food.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(food.pc$scores[,1], food.pc$scores[,2], labels=Food, cex=0.7, lwd=2,
     col=my.color.vector)

# Cluster 1 is the "canned seafood" cluster.  (this comment does not reflect all runs of the algorithm)
# Cluster 2 is the clams cluster.  (this comment does not reflect all runs of the algorithm)

## NOTE:  The default for the kmeans function in R is the Hartigan-Wong (1979) algorithm.
## The MacQueen algorithm (1967) can be used by altering the code to, say:
##                kmeans(food.std, centers=4,algorithm="MacQueen")
## You can try it in this case -- I don't think the MacQueen algorithm produces as good of a result.



##########################
##
##  K-medoids clustering
##
##########################




###########################################
###  Cars example 
###########################################


# Consider the cars.data and cars.std data frames we created above.

# Let's cluster the cars into k groups using the K-medoids approach.

# The function "pam" is in the "cluster" package.

# Loading the "cluster" package:

library(cluster)

# K-medoids directly on the (standardized) data matrix:
cars.kmed.3 <- pam(cars.std, k=3, diss=F)

# Or you can do K-medoids by inputting the distance matrix:
# cars.kmed.3 <- pam(dist.cars, k=3, diss=T)

cars.kmed.3$clustering  # printing the "clustering vector"

cars.kmed.3$silinfo$avg.width  #printing the average silhouette width

### A little function to calculate the average silhouette width
### for a variety of choices of k:

my.k.choices <- 2:8
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(cars.std, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )

# A LARGE average silhouette width indicates that the observations are properly clustered.

# Maybe k=2 is the best choice of k here?


cars.3.clust <- lapply(1:3, function(nc) row.names(cars.data)[cars.kmed.3$clustering==nc])  
cars.3.clust   # printing the clusters in terms of the car names


# Cluster 1 seems to be mostly compact cars, Cluster 2 is sports cars, Cluster 3 is large luxury sedans

############# Visualization of Clusters:

## Built-in plots available with the pam function:

# The "clusplot":

plot(cars.kmed.3, which.plots=1)

# The clusplot (in the "cluster" library) can actually be used with 
# any clustering partition by entering the data set and the clustering vector, e.g.:

clusplot(food[,-1], food.k4$cluster)

# The "silhouette plot":

plot(cars.kmed.3, which.plots=2)

# This shows which observations are "best clustered."





####################################################
####################################################
# 
# Choosing the number of clusters k using the average silhouette width criterion.
#
####################################################
####################################################

# When using pam, the output will give you the average silhouette width (see above code).

# We can also get the average silhouette width when using other algorithms:

### With a hierarchical method (Complete linkage here):

dist.food <- dist(food.std)
food.complete.link <- hclust(dist.food, method='complete')

summary(silhouette(cutree(food.complete.link, k=2), dist(food.std)))$avg.width
summary(silhouette(cutree(food.complete.link, k=3), dist(food.std)))$avg.width
summary(silhouette(cutree(food.complete.link, k=4), dist(food.std)))$avg.width
summary(silhouette(cutree(food.complete.link, k=5), dist(food.std)))$avg.width

### With k-means:

summary(silhouette(kmeans(food.std, centers=2, iter.max=100, nstart=25)$cluster, dist(food.std)))$avg.width
summary(silhouette(kmeans(food.std, centers=3, iter.max=100, nstart=25)$cluster, dist(food.std)))$avg.width
summary(silhouette(kmeans(food.std, centers=4, iter.max=100, nstart=25)$cluster, dist(food.std)))$avg.width
summary(silhouette(kmeans(food.std, centers=5, iter.max=100, nstart=25)$cluster, dist(food.std)))$avg.width

# In each case, we might choose the value of k associated with the LARGEST average silhouette width.




################################
################################
## 
##  Plotting the WSS for several choices of k
##
################################
################################

# This is a recommended method for choosing k in K-means clustering.

# For the cars data, let's consider letting k vary up to 5.

### CODE FOR WSS PLOT BEGINS HERE ###
##
#Enter name of the data matrix to be clustered here:
my.data.matrix <- cars.std  

my.k.choices <- 2:5
n <- length(my.data.matrix[,1])
wss1 <- (n-1)*sum(apply(my.data.matrix,2,var))
wss <- numeric(0)
for(i in my.k.choices) {
  W <- sum(kmeans(my.data.matrix,i)$withinss)
  wss <- c(wss,W)
}
wss <- c(wss1,wss)
plot(c(1,my.k.choices),wss,type='l',xlab='Number of clusters', ylab='Within-groups sum-of-squares', lwd=2)
##
### CODE FOR WSS PLOT ENDS HERE ###

# For what value of k does the elbow of the plot occur




















#################################################################
#################################################################
###########
###########           Model-based Clustering        
###########
#################################################################
#################################################################

# Consider the built-in USArrests data set in r:

help(USArrests)

# We will perform a model-based clustering of the 50 states based on these 4 variables:

# Loading the mclust package:
# May need to install the mclust package first?
# If so, type at the command line:  install.packages("mclust", dependencies=T)
# while plugged in to the internet.

library(mclust)

# The R function Mclust performs model-based clustering for a range of models
# and a variety of values of k:

arrest.clus <- Mclust(USArrests)

# By default, the models considered are:
# "EII": spherical, equal volume 
# "VII": spherical, unequal volume 
# "EEI": diagonal, equal volume and shape
# "VEI": diagonal, varying volume, equal shape
# "EVI": diagonal, equal volume, varying shape 
# "VVI": diagonal, varying volume and shape 
# "EEE": ellipsoidal, equal volume, shape, and orientation 
# "EEV": ellipsoidal, equal volume and equal shape
# "VEV": ellipsoidal, equal shape 
# "VVV": ellipsoidal, varying volume, shape, and orientation  

# Plotting the BIC values:

plot(arrest.clus, data=USArrests, what="BIC")

# Hit ENTER to see the BIC plot.

# The best solution is VEI with 3 clusters.

# The clustering vector:

clus.vec.3 <- arrest.clus$classification
clus.vec.3

arrest.3.clust <- lapply(1:3, function(nc) row.names(USArrests)[clus.vec.3==nc])  
arrest.3.clust   # printing the clusters in terms of the state names

# This gives the probabilities of belonging to each cluster for every object:

round(arrest.clus$z,2)


# Visualizing the clusters:

## Via a scatterplot matrix:

plot(arrest.clus, what="classification")

# Hit ENTER to see a scatterplot matrix with the points separated by cluster.

### Via a plot of the scores on the first 2 principal components, 
### with the clusters separated by color:

arrests.pc <- princomp(USArrests,cor=T)

# Setting up the colors for the 5 clusters on the plot:
my.color.vector <- rep("blue", times=nrow(USArrests))
my.color.vector[arrest.clus$classification==2] <- "red"
my.color.vector[arrest.clus$classification==3] <- "green"

# Plotting the PC scores:

par(pty="s")
plot(arrests.pc$scores[,1], arrests.pc$scores[,2], ylim=range(arrests.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(arrests.pc$scores[,1], arrests.pc$scores[,2], labels=row.names(USArrests), 
     cex=0.7, lwd=2, col=my.color.vector)

# Reviewing the PCA:

summary(arrests.pc,loadings=T)

# Note PC1 is an overall "lack-of-crime" index and PC2 is a "rural" index.

## Note: We could also specifically request the best, say, 2-cluster solution (according to BIC)
## if we wanted to, for example:

# arrest.clus.2 <- Mclust(USArrests, G=2)

# To see the resulting clustering vector, type:
# arrest.clus.2$classification
#
# and all the analagous plots could be done similarly as above.

## Note: Similarly, we could specifically request a specific covariance structure
## if we wanted to, for example:

# arrest.clus.EEI <- Mclust(USArrests, modelNames="EEI")

# To see the resulting clustering vector, type:
# arrest.clus.EEI$classification
#
# and all the analagous plots could be done similarly as above








#################################################################
#################################################################
###########
###########           Clustering Binary Data  
###########
#################################################################
#################################################################

# We can read the ACT math test items data from the Internet:

ACTitems <- read.table("http://www.stat.sc.edu/~hitchcock/ACTitems.txt", header=F)

# The first column is a set of labels for the 60 test items.
# The next 60 columns are the scores (0=incorrect, 1=correct) on the items for 55 male students.

# We wish to cluster the items into groups based on the students' scores on them.

# Just using squared Euclidean distance (counting total mismatches):

dist.items <- dist(ACTitems[,-1], method='euclidean')^2

dist.items.2 <- dist(ACTitems[,-1], method='binary') # This distance measure ignores 0-0 matches altogether

dist.items.3 <- dist(1 - ACTitems[,-1], method='binary') # This distance measure ignores 1-1 matches altogether


#### Complete linkage clustering of the 60 test items:

items.complete.link <- hclust(dist.items, method='complete')

# Plotting the complete linkage dendrogram:

plclust(items.complete.link, labels=ACTitems[,1], ylab="Distance")

# Single linkage:

items.sing.link <- hclust(dist.items, method='single')
plclust(items.sing.link, labels=ACTitems[,1], ylab="Distance")

# Single linkage really breaks down when there are a lot of ties among the distances!

#### K-medoids clustering of the 60 test items:

library(cluster)

### A little function to calculate the average silhouette width
### for a variety of choices of k:

my.k.choices <- 2:8
avg.sil.width <- rep(0, times=length(my.k.choices))
for (ii in (1:length(my.k.choices)) ){
  avg.sil.width[ii] <- pam(dist.items, k=my.k.choices[ii])$silinfo$avg.width
}
print( cbind(my.k.choices, avg.sil.width) )

# Maybe 2 clusters, or at most 3, should be used.

items.kmed.2 <- pam(dist.items, k=2, diss=T)
items.2.clust <- lapply(1:2, function(nc) ACTitems[,1][items.kmed.2$clustering==nc])  
items.2.clust 

items.kmed.3 <- pam(dist.items, k=3, diss=T)
items.3.clust <- lapply(1:3, function(nc) ACTitems[,1][items.kmed.3$clustering==nc])  
items.3.clust




x <- c(17,13,12,15,16,14,16,16,18,19)
y <- c(94,73,59,80,93,85,66,79,77,91)
df <- data.frame(x,y)
ft <- lm(y~x,data=df)
summary(ft)

pred <- predict(ft,data=df)
ye <- y - pred
ye2 <- ye ^ 2
Se <- sqrt(sum(ye2)/(length(y) - 1 - 1))
Se





m_

# Demo Reg

#clean workspace
rm(list = ls()) 

rm(list=ls())
setwd("D:/dat1")
hdf <- read.csv("Housings.csv",header=T,stringsAsFactors = FALSE)
dim(hdf)
str(hdf)
head(hdf)

# split the data into training and test data set
# rm(list=rm())
set.seed(300)
ind <- sample(1:nrow(a),round(nrow(a) * 0.75))
train <- a[ind,]
test <- a[-ind,]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~ Exploratary data analysis 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dim(train)
# dim(test)
# library(corrgram)
# corrgram(train[,2:4], order=TRUE)
# 
# boxplot(train[,2:4])
# hist(train$size)

# model 1 
str(train)
fit <- lm(train$price~size,data=train)
summary(fit)


# # Perform normality test
# shapiro.test(resid(fit))
# shapiro.test(fit$residuals)
# dim(test)

# # run a prediction on the test data set .
# house.price.range <- predict(fit,newdata=test, data.frame(size=2000),interval = "confidence")
pred <- predict(fit,test)

plot(test$price,col="blue",type="l")
lines(pred,col="red",type="l")

act <- test$price
pred1 <- pred

# Model validation 
ValidModel <- function(act,pred) {
  SSE = sum((act- pred)^2)
  SST = sum((mean(act)-act)^2)
  rmse = round(sqrt(mean(act-pred)^2),3)
  rSqa = 1 - SSE/SST
  print(paste("SSE =", SSE, "SST = ",SST,"Rsquare =", rSqa,"RMSE=",rmse))
  }
ValidModel(act,pred1)



# model 2 
fit1 <- lm(train$price~.,data=train)
summary(fit1)

pred11 <- predict(fit1,test)

act <- test$price
pred11 <- pred

plot(test$price,col="blue",type="l")
lines(pred11,col="red",type="l")

ValidModel(act,pred11)

step(fit1,direction = "forward")
step(fit1,direction = "backward")
step(fit1,direction = "both")


# model 3 
fit2 <- lm(train$price~size+uds,data=train)
summary(fit2)

pred12 <- predict(fit2,test)

act <- test$price
pred12 <- pred12

plot(test$price,col="blue",type="l")
lines(pred12,col="red",type="l")

ValidModel(act,pred12)


step(fit1,direction = "both")

m_

# ---------------------------------------------------
# Lasso Regression 
-------------------------------------------------
  # load the package
  # install.packages("lars")
library(lars)
# load data
str(hdf)
x <- as.matrix(train[,1:2])
y <- as.matrix(train[,4])

xt <- as.matrix(test[,1:2])
yt <- as.matrix(test[,4])


# fit model
fit <- lars(x, y, type="lasso")
# summarize the fit
summary(fit)
# select a step with a minimum error
best_step <- fit$df[which.min(fit$RSS)]
# make predictions
pred7 <- predict(fit, xt, s=best_step, type="fit")$fit
# summarize accuracy
rmse <- mean((y - pred7)^2)
print(rmse)

plot(test$price,col="blue",type="l")
lines(pred7,col="red",type="l")


x11 <- as.matrix(train[,2:3])
y11 <- as.matrix(train[,4])

xt11 <- as.matrix(test[,2:3])
yt11 <- as.matrix(test[,4])


fit <- lars(x11, y11, type="lasso")
# summarize the fit
summary(fit)
# select a step with a minimum error
best_step <- fit$df[which.min(fit$RSS)]
# make predictions
pred8 <- predict(fit, xt11, s=best_step, type="fit")$fit
# summarize accuracy
rmse11 <- mean((yt11 - pred8)^2)
print(rmse11)

plot(test$price,col="blue",type="l")
lines(pred8,col="red",type="l")

#-------------------------------------------------
# ridge Regression - penalizing the multic
#-------------------------------------------------
par(mfrow=c(2,2))
# load the package
# install.packages("glmnet")
library(glmnet)
# load data
y <-c("test$price")

str(train)
x <- as.matrix(train[,1:2])
y <- as.matrix(train[,4])

xt <- as.matrix(test[,1:2])
yt <- as.matrix(test[,4])


# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0, lambda=0.001)
# summarize the fit
summary(fit)

# make predictions
pred5  <- predict(fit, xt, type="link")

# summarize accuracy
rmse1 <- mean((yt - pred5)^2)
print(rmse1)

plot(test$price,col="blue",type="l")
lines(pred5,col="red",type="l")

x1 <- as.matrix(train[,2:3])
y1 <- as.matrix(train[,4])

xt1 <- as.matrix(test[,2:3])
yt1 <- as.matrix(test[,4])


# fit model
fit <- glmnet(x1, y1, family="gaussian", alpha=0, lambda=0.001)
# summarize the fit
summary(fit)

# make predictions
pred6  <- predict(fit, xt1, type="link")
# summarize accuracy
rmse2 <- mean((yt1 - pred6)^2)
print(rmse2)
cbind(pred5,pred6)

plot(test$price,col="blue",type="l")
lines(pred6,col="red",type="l")


-------------------------------------------------
  
  # m_..
  #---------------------------Robust Regression ---------------
  
rm(list=ls())
# robust linear regression, controlling for heteroskedasticity
# install.packages("rlm")
library(MASS)
# reg1.robust <- rlm(Sales ~ TV + Adwords, data = tr)
reg1.robust <- rlm(price ~ size + uds+park, data = train)
summary(reg1.robust)

pred10 = predict(reg1.robust,newdata=test,level=0.9,interval="confidence")


plot(test$price,col="blue",type="l")
lines(pred10,col="red",type="l")


dgeom(4,0.25)


