rm(list=ls(all=TRUE))
library(dplyr)
library(DT)
library(ggplot2)

mkt <- read.csv("D:/dat1/retailMarketing29jun.csv",sep=',')
str(mkt)
names(mkt)
class(mkt)

head(mkt)

dplyr::glimpse(mkt)
summary(mkt)
attach(mkt)
hist(AmountSpent)
str(mkt)
View(mkt)
mkt$Married
summary(mkt$Married)
mkt %>% arrange(AmountSpent)

mkt$Gender <- as.factor(mkt$Gender)
mkt$Married <- as.factor(mkt$Married)
mkt$Close <- as.factor(mkt$Close)
mkt$Own.Home <- as.factor(mkt$Own.Home)

str(mkt)
# ~~~~~~~~~~~~~~~~~~~~~~~~~
#  Data Exploration
# ~~~~~~~~~~~~~~~~~~~~~~~~~
head(mkt)
tail(mkt)

head(select(mkt,Age, AmountSpent,Married))
# mkt[rows,columns]
head(mkt[mkt$Age < 30,c("Children",'Salary')])
#  ls -ltr | grep walm*
filter(mkt,(Age > 30 & Married == 0))

filter(mkt,(Salary >= 1000 & AmountSpent > 2000))

# To select a  specific column

head(select(mkt,-Married))

str(mkt)

head(select(mkt,-c(Married,History)))

select(mkt,starts_with("Clo"))

# Adding a negative sign before starts_with() 
# implies dropping the variables starts with 'Y'

mydata33 = select(mkt, -starts_with("Sing"))

select(mkt, -starts_with("Sing"))

head(select(mkt,Married:Salary))

filter(mkt, Salary >= 10000)

filter(mkt, Salary >= 10000, Married == 0)

filter(mkt, Salary >= 10000 & Married == 'Single')

# filter(mkt, order %in% c('Location'))


# To arrange (or re-order) rows by a particular column
# such as the taxonomic order, list the name of the
# column you want to arrange the rows by

# The grepl function is used to search for pattern matching. In the following code, we are looking for records wherein column state contains 'Ar' in their name.
mydata10 = filter(mkt, grepl("$", AmountSpent))

funs(n(# Summarize Multiple Variables
  summarise_at(mkt, vars(Salary, AmountSpent), 
  ), mean, median))

# Summarize with Custom Functions
summarise_at(mkt, vars(Salary, AmountSpent),
             funs(n(),
                  missing = sum(is.na(.)),
                  mean(., na.rm = TRUE),
                  median(.,na.rm = TRUE)))

# How to apply Non-Standard Functions

# Suppose you want to subtract mean from its original value and then calculate variance of it.

set.seed(222)
mydata <- data.frame(X1=sample(1:100,100), X2=runif(100))
summarise_at(mydata,vars(X1,X2), function(x) var(x - mean(x)))

# Summarize all Numeric Variables
# The summarise_if function allows you to summarise conditionally.
summarise_if(mydata, is.numeric, 
             funs(n(),mean,median))


# Suppose you need to sort one variable by descending order and other variable by ascending oder.
arrange(mkt, desc(Salary), AmountSpent)

mkt %>% 
  summarise(avg_Salary = mean(Salary), 
            min_Salary = min(Salary),
            max_aspent = max(AmountSpent),
            total = n())

# The group_by() verb is an important function in dplyr. 
# As we mentioned before it's related to concept of
# "split-apply-combine". We literally want to split the
# data frame by some variable (e.g. taxonomic order), apply a function to the individual data frames and then combine the output.

prev_cust <- mkt %>% 
    select(Children,Salary,PrevCust,AmountSpent) %>%
    group_by(ChildrenPrevCust ) %>%
  summarise(avg_Salary = mean(Salary), 
            min_Salary = min(Salary),
            max_aspent = max(AmountSpent),
            total = n()) 

ggplot(prev_cust, aes(x=Children, 
                   y=total, fill=as.factor(PrevCust))) +
  geom_tile(color = "blue") +
  scale_fill_manual(values = c('red','blue','yellow'))

 mkt %>% 
    select(Children,Salary,PrevCust,AmountSpent) %>%
    group_by(Children,PrevCust ) %>%
  summarise(avg_Salary = mean(Salary), 
            min_Salary = min(Salary),
            max_aspent = max(AmountSpent),
            total = n()) %>%
 ggplot(aes(x=Children, 
                   y=total, fill=as.factor(PrevCust))) +  #fill = hue
     geom_tile(color = "blue") +
  scale_fill_manual(values = c('red','blue','yellow'))
 
#? scale_fill_manual


# ggtitle("Bar Chart  by Total Amount Spent by Parents by History")


  
mkt %>% arrange(Salary) %>% head


out <- boxplot(AmountSpent~mkt$Married,data = mkt,
               main="Amount Spent by Married vs Single",
               xlab="Married",ylab="Amount Spent",
               horizontal = TRUE)

out$stats
dfSingle <- mkt[mkt$Married == 'Single',]
dfMarried <- mkt[mkt$Married == 'Married',]

summary(dfSingle)

summary(dfMarried$AmountSpent)

# Column slice
mkt[c("Children","AmountSpent")]

table(mkt$Children)
table(mkt$History)


# similar to table()
mkt %>% 
  group_by(History) %>%
  tally()

table(mkt$History)


child0 <- mkt %>% 
  select(Children,AmountSpent) %>%
  group_by(Children) %>%
  summarise(Total=sum(AmountSpent), 
            avgSp=mean(AmountSpent))
child0

mkt %>% head(select(Married, starts_with("Sing")))

# select Own.Home,Married, sum(AmountSpent)
# from mkt 
# group_by(Own.Home);
select(mkt,Own.Home,Married, AmountSpent) 
  

mkt %>%  #pipe #  # ls -ltr | grep walm*
  select(Own.Home,Married, Age,AmountSpent) %>%
  filter(Age < 30) %>%
  group_by(Own.Home) %>%
  summarise(sum(AmountSpent), avg_aspent=mean(AmountSpent))


# ls -ltr | ps -grep "mkt*" 

mkt %>% 
  select(Married, AmountSpent) %>%
  group_by(Married) %>%
  summarise(sum(AmountSpent))

mkt %>% 
  select(Own.Home,Married, AmountSpent) %>%
  group_by(Own.Home,Married) %>%
  summarise(sum(AmountSpent))


mkt %>% 
  select(Own.Home,Married, Children, AmountSpent) %>%
  group_by(Own.Home,Married, Children) %>%
  summarise(sum(AmountSpent))


aspendKids <-   mkt %>% 
  select(Married, Children, AmountSpent) %>%
  filter(Married == 1) %>%
  group_by(Children) %>%
  summarise(Total= sum(AmountSpent))

aspendKids

# grammar of graphics 
ggplot(aspendKids, aes(x=factor(Children) , 
                       y=Total) +
         geom_boxplot(stat='boxplot'))



dev.off()

# arrange(Location,History,desc(AmountSpent))  

child1 <- mkt %>% 
  select(Children,History,AmountSpent) %>%
  filter (History != 'NA') %>%
  group_by(Children,History) %>%
  summarise(Total=sum(AmountSpent))

datatable(child1)



#  scale_fill_manual(values = c("blue","red")))
# ggtitle("Heat Map by Total Amount Spent by Age Group")


ggplot(child1, aes(x=Children, 
                   y=Total, fill=History)) +
  geom_tile(color = "blue") +
  scale_fill_manual(values = c('red','blue','yellow'))
# ggtitle("Bar Chart  by Total Amount Spent by Parents by History")