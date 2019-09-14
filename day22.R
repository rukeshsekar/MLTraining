getwd()  # import os  os.getcwd()  os.chdir()
setwd("D:/dat1")
setwd("D:\\dat1")
getwd()
##############################Basic calculations##############################               

x = 2
X <- 12
first_name <- 'Rajesh'
a <- as.integer(a)
l <- c(TRUE,FALSE)
class(l)
is.character(a)
a1 = 5
is.vector(a1)
class(a) # float
a = as.integer(a)
class(a)

a = c(5,4,'Ashok',10.23)
class(a)
class(a[2])
is.vector(a)


a1 <- c(2,3,4,5)
a2 <- c(1,2,3,4)
a1+a2
a1 * 5

length(a2)
prod(a2) # factorial  multiplication
cumsum(a2) # cumulative sum (running total)

a2
names(a2) <- c('Englig','sdf','sdf','math')
attributes(a2) # to display the meta data
as.numeric('12RT')

l1 <- list(12,2,4,'Ashok')
class(l1)
is.character(l1[4])


l2 <- list(2,4,6,8)
l2 * 2

l3 <- list(3,6,9,12)
l3

union(l2,l3)
setdiff(l3,l2)


for (i in l2){
  print (i * 10)
}



a1 * 2


      
l3 <- list(23,2)
l5 <- list(l1,l3,c(2,3,4))
l5

l2<- as.vector(l1)
l11 <- unlist(l2,use.names=FALSE)
is.vector(l11)


a1 <- array(c(2,4,6,8,10,12),c(3,2))
dim(a1)
a1 *2
a2 <- array(c(1,2,1,1,3,2),c(3,2))
a1+a2

d1 <- list(2,4,6,8)
d2 <- list(1,3,5,7)
d1+d2
d1 * 2
d3=d2
d3
# demonstrate asssignment operations 
'''
asdsad
aada
asd
ads
'''

a <- c(2,3,4)
a
is.vector(a)
class(a)  # type(a)
mode(a)
str(a)
typeof(a)




2+2


#Mathematical Operations
2+2
2+3^2
(2+2)^4
sqrt(2)
log(2)  # default base is exp(1)

# Assiging a value and performing operations
x = 5
x=6
y = 10
z <- x+y
z
#To create sequence of nummbers
seq(1,5, by=.5)
#Repeating a number
rep(1,10)


#############################   Evaluation and Printing    #########################################################
# The <- symbol is the assignment operator.
x <- 5 	## nothing printed  
x 		## auto-printing occurs
print(x) 	## explicit printing
# The : operator is used to create integer sequences.
x <- 1:20
x

##################################### Creating Vectors######################################
# The c() function can be used to create vectors of objects.
x <- c(0.5, 0.6) ## numeric
x <- c(TRUE, FALSE) ## logical
x <- c(T, F) ## logical
x <- c("a", "b", "c") ## character
x <- 9:29 ## integer
x <- c(1+0i, 2+4i) ## complexclass(x)
#Using the vector() function
x <- vector("numeric", length = 10)
x
x1 <- vector(10)
x1

? vector
#################################  Mixing Objects ##################################
y <- c(1.7, "a") ## character
y <- c(TRUE, 2) ## numeric
y <- c(TRUE, 2,FALSE) ## numeric
y <- c("a", TRUE) ## character
###############################  Explicit Coercion #########################
x <- 0:6
class(x)
as.numeric(x)
as.logical(x)
as.character(x)

#Coercion results in NAs.
x <- c("a", "b", "c")
as.numeric(x)
as.logical(x)
x <- c("a", "b", "c", 3)
as.numeric(x)

###########################################  Matrices  ########################
m <- matrix(nrow = 2, ncol = 3)
m
dim(m)
attributes(m)
m <- matrix(1:6, nrow = 2, ncol = 3)  # here byrow=FALSE
m
m <- matrix(1:6, nrow = 2, ncol = 3,byrow = TRUE)
m
# Matrices can be constructed row-wise also by defining parameter byrow
m <- matrix(1:6, nrow = 2, ncol = 3, byrow=TRUE)
m
dim(m)

m <- 1:10
m

is.matrix(m)
m <- matrix(1:6, nrow = 2, ncol = 3)  # here byrow=FALSE
m
dim(m) <- c(3, 2)
m <- matrix(m,3,2,byrow=TRUE)
m

##################  cbind-ing and rbind-ing   ###########################################

x <- 1:3
y <- 10:12
cbind(x, y)

x 
y
rbind(x, y)

install.packages(h2o)
library(h2o)
h2o.init()

##############################  Lists ###########################

x <- list(c(1,2), "a", TRUE)
x

##############################  Factors  ##################################
x <- factor(c("yes", "yes", "no", "yes", "no"))
x

############################   Missing Values  ##############################

x <- c(1, 2, NA, 10, 3)
is.na(x)
is.nan(x)
x <- c(1, 2, NaN, NA, 3)
is.na(x)
is.nan(x)


###################   Data Frames   ###############################

x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
x
is.data.frame(x)
class(x)
nrow(x)
ncol(x)

#####################################  Names ###############################
x <- 1:3
names(x)

names(x) <- c("foo", "bar", "norf")
x
names(x)

############################ Names & list #####################
x <- list(a = 1, b = 2, c = 3)
x

########################  Names & Matrix ###########################
m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b"), c("c", "d"))
m
#############################  Sub-set of a vector ####################

x <- c(2,4,6,6,8,2)
x[1]
x[2]
x[1:4]
x[x > 2]
u <- x  >2
u
x[u]
#############################  Sub-set of a matrix ####################
#Matrices can be subset in the usual way with (i , j) type indices.
x <- matrix(1:6, 2, 3)
x[1, 2]
x[2, 1]

# row/column  Indices can also be excluded as cited below to get only columns or only rows respectively
x[1, ]

x[, 2]

###################  Removig NA values ########################

x <- c(1, 2, NA, 4, NA, 5)
x[! is.na(x)]
y=na.omit(x)
y

x[! x %in% 4]

################################  Vector Operations  #####################

x <- 1:4; y <- 6:9
x + y
x > 2
x * y


############################  Matrix Operations #########################

x <- matrix(1:4, 2, 2); y <- matrix(rep(10, 4), 2, 2)
x * y ## element-wise multiplication
x / y
x %*% y        ## true matrix multiplication

#################### Data Types (vectors, matrices & dataframes) ####################
v1=c(1,2,3,4,5) #numeric vector
v2=c("a","b","c","d","e") #character vector
v3=c(TRUE,FALSE,TRUE,FALSE,TRUE) #logical vector
str(v1) #to view structure of the vector
length(v1) #to get number of  elements in the vector
str(v2)
str(v3)
str(c("fg","gh"))
cbind(v1,v2,v3) #column binding
rbind(v1,v2,v3) #row binding

#Playing with a vector
v4 = c(10,9,8,7,6,5)
max(v4)
min(v4)
mean(v4)
sd(v4)
v4[c(1,3)]#To get 1st and 3rd item
v4[v4=8] #To get elements which are greater than 8

#Matrix and data frame
M=matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE) # to create a matrix
ls()
rm(M)
data = data.frame(v1,v2,v3) # to create a data frame
names(data)
colnames(data)
names(data) = c("ID","Name","Selected") # to assign variable names
data1 = data[,c(2:3)]
#Handling missing values in the data
v=c(10,25,20,NA,36,100)
mean(v,na.rm=TRUE)
mean(v)
is.na(v)
v[is.na(v)]=555
v

##################################  Central tendencies ############################

vec = c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,2,3,4,3,4,4)
length(vec)
mean(vec)
median(vec)
quantile(vec)
sd(vec)
var(vec)

vec = c(1,2,3,4,6,7,8)
quantile(vec)
quantile(vec,.20)
quantile(vec,.5)

##################################  Date  ############################


Sys.Date()

dt <- c("2018-02-08","2018-02-10")
dt <- as.Date(dt)
dt[2] - dt[1]

today <- Sys.Date()
format(today, format = "%B %d %Y")
format(today, format = "%d %B %Y")

# convert date info in format 'mm/dd/yyyy'
strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")


mydate = as.POSIXlt('2005-4-19 7:01:00')




EMI = P * R * (1+R)^n]/ [(1+R)^n-1]

4
13.75
5

((4000000 * 13.75) * (1+13.75)^5 ) /  (1+ 13.75)^4

13.75/100

((4000000 * .1375) * (1+.1375)^5 ) /  (1+ .1375)^4



Cemi <- function(p,r,n) {
  res = ( p *  r * (1+r)^5 ) / (1 + r)^n-1
  return(res)
}

NetOut <- function(p1,p,r,n) {
  res1 =  Cemi(p,r,n)
  return(res1)
}

Cemi(400000,.1375,5)

Cemi(40000:50000,.1375,5)

NetOut(40000,40000,0.1375,5)


m <- matrix(1:10, 2)
a <- array(1:10, c(2, 5))
identical(m, a)


# An array is a vector with one or more dimensions. So, an array with one
# dimension is (almost) the same as a vector. An array with two dimensions is
# (almost) the same as a matrix. An array with three or more dimensions is
# an n-dimensional array.

x = array(1:24, c(3,4,2))
x

x[1,3,2]

x[2]
x[2:3]


# no.of rows, no. of columns
m <- matrix(1:10, 2)
m
dim(m)
m <- matrix(1:20, 2)
m

m <- matrix(1:24, 3,4)
m
dim(m)

m <- matrix(1:10, 2,4, dimnames = list(c("r1","r2")))
m[1,2]
m[,2]
m[2,]

v <- c(1:24)
v

identical(x,v)

# Anonymous function in R
(function(x) x * 10) (10)

(function(x,y) x + (y * 10)) (10,20)


(function(x) x * 10)(10)




toda = Sys.Date()
format(toda,format = "%d %B %y")



