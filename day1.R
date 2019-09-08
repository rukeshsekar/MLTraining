library(car)
library(ggplot2)  # import ggplot, pandas, numpy
library(caret)

pkgs = c('ggplot2','cluster','randomForest')
install.packages(pkgs)

getwd() # in  pyton , import os   os.getcwd()

setwd("D:/dat1")
getwd()

df = read.csv("./Sales.csv")       # pd.read_csv()
df1 = read.csv("./housing.csv")
class(df)

head(df)  # df.head()
tail(df)


x <- 10
class(x)   # in Pythh
mode(x)
str(x)

x1 <- as.integer(x)
class(x1)
str(x1)



a = 10.33
class(a)
as.integer(a)
ceiling(a) # ceil(), floor()
floor(a)

# character data type
s = 'SauriNathan'
class(s)  # type(s)  str


# logical data type   
TRUE==TRUE  # in Python , True, False

x = 2
y = 3  
x + y

# how to define a vector
# using c() we define a vector
a1 = c(10,8,12.2)
class(a1)
is.vector(a1)

a2 = c(0,8,2)  # like python , R is also case sensitive
A2 = c(10,2,4)
a2
A2

a2 = c(0L,8L,2L)   # integer vector
class(a2)

s1 = c('Ashok','Sheetal','Vetri', 'Satheesh')
class(s1)  # character vector

b = c(TRUE, FALSE)
class(b) # logical vector

m = c(2L,'Mahesh',TRUE, 3.25) # 
class(m)  # order of precedence
m

m = c(2L,TRUE, 3.25) # 
class(m)  # order of precedence

m = c(2L,TRUE, FALSE) # 
class(m)  # order of precedence
m

a2 * 10
a2 + 10
length(a2) # len
class(a2)
min(a2)
a2
max(a2)
median(a2)
sd(a2)
cumsum(a2)
a2
attributes(2)
names(a2)  # column names

names(a2) <- c('Samsung','Nokia','Motorola')
a2
names(a2)
attributes(a2)  # meta data (data about data)

a = list(10,20,3,4) # to define a list in python, []
l1 = list(11,12,13)

l2 = list(a,l1)
l2
l2[[1]][[1]]


































































