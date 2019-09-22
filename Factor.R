m_

# import pandas as pd
# 

df <- read.csv("D:/dat4/CustomerData1.csv",sep=',')
df

str(df)
nrow(df)

colnames(df)

df[,c(7,8)]
apply(df[,c(7,8)],1,mean)
apply(df[,c(7,8)],2,mean)

x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
x
apply(x,1,mean)
apply(x,2,mean)

1:8

fn <- function(x){
  res = x ^2 
  return(res)
}
fn(1:8)

x1 = sapply(1:8, function(x) x^2)
X2= list(x1)
class(X2)

x1 = lapply(1:8, function(x) x^2)

x1 = sapply(df$NoOfGamesPlayed, function(x) x^2)
x1

data(mtcars)
str(mtcars)

tapply(mtcars$mpg, list(mtcars$cyl,mtcars$am),mean)

mapply(sum,1:3,1:3,1:3)







region <- c('north','south','east','west','west','south')
region 
length(region)
is.factor(region)
is.character(region)

levels(region)

region_factor <- factor(region)
region_factor
is.factor(region_factor)

length(region_factor)

levels(region_factor)

region_factor[1] > region_factor[3]
region_factor[3]


rm(region) # uncache the variables 

region <- c('north','south','east','west','west','south')
region 

levels(region) <- c('north','south','west','east')

levels(region)
unclass(region)

table(region)

# you can also replace the factor values
# alternatievly 

levels(region_factor) <- c('S','W','N','E')

region <- c('South','West','North','East')

reg <- factor(region,levels = c('E','S','W','N'),
              labels =c('East','South','West','North') )
reg


region <- c('north','south','east','west','west','south')
is.vector(region)
reg <- factor(region)
levels(reg)
reg
unclass(reg)



unclass(reg)
reg[1] < reg[2]

reg <- factor(region,levels = c('West','South','North','East'),
              labels = c('W','S','N','E'))
unclass(reg)

-------------------------------------
  
region_factor1 <- factor(region, levels = c('east','west','north','south'))
region_factor1
levels(region_factor1)
unclass(region_factor1)

region_factor1[1] > region_factor1[2]
-------------------------------------
  
  
  levels(region)

region <- c('N','S','W','E')
region 

reg <- factor(region,levels = c('N','S','W','E'),
              labels = c('North','South','West','East'))

unclass(reg)
reg[1] < reg[2]

------------------
defect_types <- c("High",'Medium','Low')       

defect_types <- factor(c("High",'Medium','Low'))       
defect_types
levels(defect_types)
unclass(defect_types)

defect_types <- factor(c("Low",'Medium','High'),
                       levels = c("High",'Medium','Low'))
defect_types

unclass(defect_types)
levels(defect_types)

defect_types[3] < defect_types[2]

rm(defect_types1)

defect_types <- factor(c("High",'Medium','Low'))       

defect_types2 <- factor(defect_types,
                        ordered = TRUE
                        )

unclass(defect_types2)

defect_types1 <- factor(defect_types,
                        ordered = TRUE,
                        levels = c("Low",'Medium','High'))

defect_types1
levels(defect_types1)
unclass(defect_types1)

defect_types1[3] < defect_types1[1]

--------------------------------
sales <- c(20000,30000,31100,25900,26800,31180)
region <- c('north','south','east','west','west','south')
df <- data.frame(sales,region)
str(df)
levels(df$region)
unclass(df$region)
# One quick way to get rid of this is to use the
# droplevels() function to drop all unused levels from your dataset.

df1 <- subset(df,region != 'north')
df1
str(df1)
levels(df1$region)

df1 <- droplevels(df1)
levels(df1$region)
unclass(df1$region)



l1 = list(2,3,4)
l2 = list(3,4,9)
l1 + l2
