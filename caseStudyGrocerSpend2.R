# Om Gam Ganapataye Namah
# Om Gurave Namah
# Om Swami malai muruga potri
# Om hayagreeva Potri

# rsconnect::setAccountInfo(name='dssasken',
#                           token='091140EFFB5A46DC5CA57C18D0E0B6EF',
#                           secret='Q5nCROjiS7wRM80ftTFkEqk61f44kbgbKBjCdPmt')
# library(rsconnect)
# rsconnect::deployApp('path/to/your/app')
rm(list=ls(all=TRUE)) 
library(dplyr) 
library(DT)
library(ggplot2)

# mkt <- read.csv("D:/dat1/retailMarketing29jun.csv",sep=',')
mkt <- read.csv("D:/dat1/retailMarketingDI.csv",sep=',')
mkt <- na.omit(mkt)

str(mkt)

names(mkt)
class(mkt)

head(mkt)

#dplyr::glimpse(mkt)
glimpse(mkt)
summary(mkt)
attach(mkt)
hist(AmountSpent)
str(mkt)
View(mkt)
mkt$Married
summary(mkt$Married)

mkt %>% arrange(AmountSpent)
# ~~~~~~~~~~~~~~~~~~~~~~~~~
#  Data Exploration
# ~~~~~~~~~~~~~~~~~~~~~~~~~
head(mkt)
tail(mkt)

filter(mkt,(Age == 'Old'  & AmountSpent > 2000))

filter(mkt,(Age < 30 & Married == 0))

filter(mkt,(Salary >= 1000 & AmountSpent > 2000))
str(mkt)

mkt[(mkt$Age == 'Old' & mkt$AmountSpent > 2000),c('AmountSpent','Age')]

# To select a  specific column

head(select(mkt,-Married))

str(mkt)
select(mkt,c(Married, AmountSpent))
 
# ls - ltr | grep sample*

mkt %>%
  select(Age,Married, AmountSpent) %>%
  filter(mkt,Married == "Married" & Age == 'Old')
  

mkt %>%
  select(Age,Married,History, AmountSpent) %>%
  filter(Married == "Married" & Age == 'Old') %>%
  group_by(History) %>%
  summarise(TotalAmountSpent = sum(AmountSpent,
                                   Noofrecs=n()))



head(select(mkt,-c(Married,History)))

select(mkt,starts_with("Marr"))

# Adding a negative sign before starts_with() 
# implies dropping the variables starts with 'Y'

mydata33 = select(mkt, -starts_with("Sing"))

select(mkt, -starts_with("Single"))

head(select(mkt,Married:Salary))

filter(mkt, Salary >= 10000)

filter(mkt, Salary >= 10000, Married == 'Single')

filter(mkt, Salary >= 10000 & Married == 'Single')

# filter(mkt, order %in% c('Location'))


# To arrange (or re-order) rows by a particular column
# such as the taxonomic order, list the name of the
# column you want to arrange the rows by

# The grepl function is used to search for pattern matching. In the following code, we are looking for records wherein column state contains 'Ar' in their name.
mydata10 = filter(mkt, grepl("$", AmountSpent))

  funs(n(# Summarize Multiple Variables
summarise_at(mkt, vars(Salary, AmountSpent), 
           , mean, median))
)
  
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
  select(AmountSpent,History) %>%
  group_by(History) %>%
    summarise(Total= sum(AmountSpent))


mkt %>%
  select(AmountSpent,History,Age) %>%
  group_by(History,Age) %>%
  top_n(2)


mkt %>% 
  filter(Gender == 'Male') %>%
  summarise(avg_Salary = mean(Salary), 
            min_Salary = min(Salary),
            max_aspent = max(AmountSpent),
            total = n())

summary(mkt$Gender)

# The group_by() verb is an important function in dplyr. 
# As we mentioned before it's related to concept of
# "split-apply-combine". We literally want to split the
# data frame by some variable (e.g. taxonomic order), apply a function to the individual data frames and then combine the output.

# select count(*) 

mkt %>% 
  group_by(History )
  


mkt %>% 
  group_by(History ) %>%
  summarise(avg_Salary = mean(Salary), 
            min_Salary = min(Salary),
            max_aspent = max(AmountSpent),
            total = n())

mkt %>% 
  select(History,AmountSpent) %>%
  group_by(History ) 
  

mkt %>% 
  select(History,Age, AmountSpent) %>%
    group_by(History , Age)  %>%
   summarise(TotalAmountSpent =sum(AmountSpent))

  

mkt %>% 
  select(History,Salary, AmountSpent) %>%
  group_by(History ) %>%
  summarise(avg_Salary = mean(Salary), 
            min_Salary = min(Salary),
            max_aspent = max(AmountSpent),
            total = n())


mkt %>% arrange(-Salary) %>% head

out <- boxplot(AmountSpent~mkt$Married,data = mkt,
               main="Amount Spent by Married vs Single",
               xlab="Married",ylab="Amount Spent",
               horizontal = TRUE)

table(mkt$History)


# similar to table()
mkt %>% 
  group_by(History) %>%
  tally()

    mkt %>% 
    select(Children,AmountSpent) %>%
      group_by(Children) %>%
      summarise(Total=sum(AmountSpent), 
                avgSp=mean(AmountSpent))
  
      mkt %>% 
       select(OwnHome,Married, AmountSpent) %>%
       group_by(OwnHome) %>%
       summarise(sum(AmountSpent))
     
     
      # ls -ltr | ps -grep "mkt*" 
      
      mkt %>% 
        select(Married, AmountSpent) %>%
        group_by(Married) %>%
        summarise(sum(AmountSpent))
      
      mkt %>% 
        select(OwnHome,Married, AmountSpent) %>%
        group_by(OwnHome,Married) %>%
        summarise(sum(AmountSpent))
      
      mkt %>% 
        select(OwnHome,Married, Children, AmountSpent) %>%
        group_by(OwnHome,Married, Children) %>%
        summarise(sum(AmountSpent))
      
      
 aspendKids <-   mkt %>% 
        select(Married, Children, History,AmountSpent) %>%
        filter(Married == 'Married') %>%
        group_by(History,Children) %>%
        summarise(Total= sum(AmountSpent))

 aspendKids
      
    # grammar of graphics 
ggplot(aspendKids, aes(factor(Children),Total))+ 
  geom_bar(stat='identity')
       

  library(DT)
  datatable(aspendKids)
    
    
   
    #  scale_fill_manual(values = c("blue","red")))
      # ggtitle("Heat Map by Total Amount Spent by Age Group")


    ggplot(aspendKids, aes(x=Children, 
                       y=(as.integer(Total)/100), fill=History)) +
      geom_tile(color = "blue") +
      scale_fill_manual(values = c('red','blue','yellow'))
      # ggtitle("Bar Chart  by Total Amount Spent by Parents by History")
    
    ggplot(child1, aes(x=Children, 
                       y=Total, fill=History)) +
      geom_bar(stat = "identity") +
      ggtitle("Bar Chart  by Total Amount Spent by Parents by History")
    
    str(mkt)
    close_spent <- mkt %>% 
      select(Catalogs,History,AmountSpent) %>%
      filter (History != 'NA') %>%
      group_by(Catalogs,History) %>%
      summarise(Total=sum(AmountSpent))
    
unique( mkt$Catalogs)
    ggplot(close_spent, aes(x=Catalogs,y=Total/100,
                            fill=History)) +
      geom_bar(stat='identity') +
      scale_fill_manual(values = c('orange','blue','yellow')) +
      ggtitle('Amount Spent by Catalogs by History')
    
        
    '''
    ggplot(close_spent, aes(x=Catalogs,y=Total/100,
                            fill=History))+
      geom_tile(color = "white")+
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab",
                           name="Pearson\nCorrelation") + # Change gradient color
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))+
      coord_fixed()
    '''
    
    summary(mkt$Children)
str(mkt)

out <- boxplot(AmountSpent~mkt$Children,
               data = mkt,
               main="Amount Spent by Parents",
               xlab="Children",ylab="Amount Spent",
               horizontal = TRUE)
out$stats

ggplot(close_spent, aes(x=Catalogs, 
                   y=Total, fill=History)) +
  geom_bar(stat = "identity") +
  ggtitle("Bar Chart  by Total Amount Spent by Catalogs by History")

ggplot(close_spent, aes(x=Catalogs, 
                        y=Total, fill=History)) +
  geom_bar(stat = "identity") +
  ggtitle("Bar Chart  by Total Amount Spent by Catalogs by History")

unique(mkt$Catalogs)
names(mkt)

loc_spent <- mkt %>% 
  select(Catalogs,History,Location,AmountSpent) %>%
  filter (History != 'NA') %>%
  group_by(Location,Catalogs,History) %>%
  summarise(Total=sum(AmountSpent))

ggplot(loc_spent, aes(x=Location,y=Total, 
                      fill= History))+
  geom_bar(stat='identity') +
  scale_fill_manual(values=c('orange','green','yellow'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   mutate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mkt %>% 
select(Salary, AmountSpent,Married) %>%
 mutate(Spend_ratio = (AmountSpent/Salary) * 100)


    #arrang * 100ied,desc(AmountSpent))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   summarize_each
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mkt %>% 
  select(AmountSpent,Salary,Married) %>%
  #  group_by(Married)  %>%
  summarise_each(funs(mean),Salary, AmountSpent)

mkt %>% 
  select(AmountSpent,Salary,Married) %>%
  #  group_by(Married)  %>%
  summarise(avg1 = mean(Salary), avg2=mean(AmountSpent))


library(foreign)
insdf <- read.spss('D:/dat1/car_insurance_claims.sav')
str(insdf)
View(insdf)
is.numeric(insdf$claimamt)
is.numeric(insdf$nclaims)

ggplot(insdf,aes(x=claimamt,y=nclaims)+
         geom_tile(color = 'Blue')
       )

library(reshape)
dim(mkt)
names(mkt)
cast(mkt,Married ~ Location)
cast(mkt,Catalogs ~ Location)



sal <- function (hra,basic){
  gsal = hra+basic

}

netSal<- function(sal,hra,basic,leave){
net = sal(hra,basic)   
net = net - leave
return(net)
}
netSal(sal,1000,2000,500)

boxplot(df$AmountSpent~df$History,data=df,
        main='Aspent by HIstory',
        xlab='AmountSpent', ylab='History',
        )


