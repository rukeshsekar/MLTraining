lossofpay = 1500
basic=10000
hra = 2000

if (lossofpay != 0) {
  netsal = (basic+hra) - lossofpay
  print(netsal)
}

# in Python
'''
if (lossofpay != 0) :
  netsal = (basic+hra) - lossofpay
  print(netsal)
'''

  
if (lossofpay != 0) {
  netsal = (basic+hra) - lossofpay
  print(netsal)
}else {
  netsal = basic+hra
  print(netsal)
}


# function
netsal <- function(b,h,l){
  if (l != 0) {
    netsal = (b+h) - l
    print(netsal)
  }else {
    netsal = b+h
    print(netsal)
  }
}

netsal(10000,2000,1500)

# in Python
def netsal(b,h,l):
  if (l != 0): 
    netsal = (b+h) - l
    print(netsal)
  else:
    netsal = b+h
    print(netsal)
  


i = 10
repeat {
  if (i > 20)
    break;
  print(i)
  i = i +1
}

i = 10
while( i != 2) {
  print(i)
  if (i == 20 ) {
    break
  }
  i = i +1
}

1:10

for ( i in 1:15){
    if (i == 10){
    break
    }
  print (i)
}

for ( i in 1:15){
  print (i)
  if (i == 10){
    break
  }
}


for ( i in 1:10){
  if (i %% 2 == 1 ) {
    print(i)
    print ("")
  } else (i %% 2 == 0){
    print(i)
  }
}


df = read.csv("D:/dat1/sales.csv", sep=',')
class(df)
head(df)
tail(df)
str(df)

df[, c('region','sales')]
# in python,  df[,['region','sales']]

df[df['sales'] > 10000,c('region','sales')]
#df[df['sales'] > 11001, ['region','sales']]
















x = 10
i = 1
repeat {
  i <- i + 1
  y <- x + 1
  print(y)
  if (i > 20 ){
    break
  }
 y 
}


x = 30
i = 1
while (x > 10 ) {
  i <- i + 1

  print(y)
  if (i > 20 )
  {
  break
  }
  y <- x + 1
  }

# take a list of 12 values and display 
# only even numbers  1:12
x %% 2 
