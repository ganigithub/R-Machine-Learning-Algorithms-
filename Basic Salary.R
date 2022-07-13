# importing datasets : csv type
## using complete path

salary <- read.csv(file = 'C:/Users/ganesh/Documents/R files/basic_salary.csv', stringsAsFactors = T)

## importing file using relative path: 
# go to Session > Set Working Directory > Choose directory > select the default path in which you want to search

salary <- read.csv(file = 'basic_salary.csv')


## using choose.file()
salary <- read.csv(file.choose())
salary

## to view data and apply filters, choose Import Dataset in Global environment and import


summary(salary)

##Basic Features

head(salary)  #0r head(salary, 10) for first 10 entries
tail(salary)


## subset: index based and condition based

# index based:

#fetch rows from 20 to 40
salary[20:40,]

#fetch first name and ba for all employees
salary[, c('ba','First_Name')]

#fetch first name and location for emp having index 10 to 21
salary[10:21, c('First_Name', 'Location')]

#fetch first name, ba and ms for emp having index 1-10, 15-20 and 33-41
salary[c(1:10,15:20,33:41),c('First_Name','ba','ms')]

#count total percentage of missing entries

is.na(salary)  # True or False for missing values
sum(is.na(salary)) #total True entries from is.na
colSums(is.na(salary))   #missing values for each columns

11 / (nrow(salary)*ncol(salary))   #3.8 % values are missing




#mean and median for ba column:
mean(salary$ba)  #since NA values are present mean, median, etc are Not Available

mean(salary$ba, na.rm = T)
median(salary$ba, na.rm = T)
var(salary$ba, na.rm = T) # squarred difference of each data w.r.t mean.
sd(salary$ba, na.rm = T) # sqrt of variance

summary(salary$ba)





### Handling Missing Values:

#impute NA with meaningful value
# na can be imputed with Central Tendency [numeric : median / category : mode]

salary$ba[is.na(salary$ba)] <- median(salary$ba, na.rm = T)
salary$ms[is.na(salary$ms)] <- median(salary$ms, na.rm =T)

#missing values in Grade
summary(salary$Grade)
salary$Grade[is.na(salary$Grade)] <- "GR1"

#missing values in Location
summary(salary$Location)
salary$Location[is.na(salary$Location)] <- 'MUMBAI'

#missing values in Function
summary(salary$Function)
salary$Function[is.na(salary$Function)] <- "SALES"

colSums(is.na(salary))  #No missing values now.





### BOXPLOT: outliers
# box - whisker
# gives distribution of data
# box contains : 1Q , 2Q and 3Q
# two whiskers: upper whisker: Q3 + 1.5IQR (InterQuartile range)
# lower whisker: Q1 - 1.5(IQR)
# IQR  =  Q3 - Q1
# Outliers: > UW or  <LW

boxplot(salary$ba)

IQR(salary$ba)   #gives value of IQR for ba
LW = 13660 - 1.5*(5575); LW  #5297.5
UW = 19234 + 1.5*(5575) ; UW #27597.5

#find the outlier in ba:
sub1 <- subset(salary, salary$ba > UW); sub1

## managing outliers: If outlier is more than UW, Replace it with UW instead of mean or median or mode
# If outlier is less than LW, replace it with LW instead of mean median or mode

#here outlier is more than UW so we replace it with UW
salary$ba[salary$ba > UW] <- UW

boxplot(salary$ba)  #no outliers


boxplot(salary$ms)
summary(salary$ms)
quantile(salary$ms) #gives quartiles directly

IQR = 13730-9300
UW <- 13730 + 1.5*IQR; UW
LW <- 9300 - 1.5*IQR; LW

sub2 <- subset(salary, salary$ms < LW); sub2

salary$ms[salary$ms < LW] <- LW
boxplot(salary$ms)

#find value of ms at 23rd percentile
quantile(salary$ms, probs= 0.23)
quantile(salary$ms, probs = c(0.9, 0.95, 0.99))

quantile(salary$ba, probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
quantile(salary$ba, probs = seq(0.1,1,0.1))

## conditional subset
# find employes having ba > median of ba

sub3 <- subset(salary, salary$ba > 15800); sub3

#find emp having location Delhi
sub4 <- subset(salary, salary$Location == 'MUMBAI'); sub4

#find First name of employes having ms< average ms

summary(salary$ms)
sub5 <- subset(salary, salary$ms < 11133); sub5
sub5['First_Name']
#or
sub5 <- subset(salary, salary$ms < mean(salary$ms), select = "First_Name");sub5

#find emp having ba > 20000 and working in Delhi:
## compound condition: Logical operator
# logical operators: and (&), or(|), not(!)

#NOT(!):
!T  #False
!F  #True

#AND(&): ALL conditions should be True
#OR (|): AT LEAST one conditions must be True

sub6 <- subset(salary, salary$ba > 20000 & salary$Location == 'DELHI'); sub6

#find emp working in finance / sales
sub7 <- subset(salary, salary$Function =='FINANCE' | salary$Function == 'SALES'); sub7
#or
sub7 <- subset(salary, salary$Function != "TECHNICAL"); sub7


sub8 <- subset(salary, (salary$ba > 15000 & salary$ms < 20000) & (salary$Function != 'TECHNICAL')); sub8


#find avg ba for complete data
mean(salary$ba)

#find avg ba w.r.t location

s1 <- subset(salary, salary$Location=='MUMBAI')
s2 <- subset(salary, salary$Location == 'DELHI')
mean(s1$ba)
mean(s2$ba)

#or using aggregate funciton:
aggr1 <- aggregate(ba~Location, data = salary, FUN = mean); aggr1

#or
tapply(salary$ba, salary$Location, mean)


#find variance of ms w.r.t Grade and location

aggr2 <- aggregate(ba~Grade + Location ,data = salary, FUN = var); aggr2

#find median of ba, ms wrt grade and location
aggr3 <- aggregate( cbind(ba,ms) ~ Grade+Location, data = salary, FUN = median); aggr3 #cbind stands for Column bind




###order()
# SORT : sorts vectors
v1 <- c(1,100,5,3,-2)
sort(v1)

#sort data with ba variable
ord1 <- salary[order(salary$ba),]; ord1
#sort data with ba variable in decreasing order
ord2 <- salary[order(-salary$ba),]; ord2

#sort data with Ascending / Descending wrt location
ord3 <- salary[order(salary$Location),]; ord3
ord3 <- salary[order(salary$Location, decreasing = T),] #for descending order
#since Loaction is factor, it will not work for descending. Only numbric data works with '-'

#arrange data wrt location and ba

# in case of location(factor) and ba(numeric), we should first sort factor
ord4 <- salary[order(salary$Location, salary$ba),]; ord4

#arrange data with decreasing order of location and ba
ord5 <- salary[order(salary$Location, salary$ba, decreasing = T),]; ord5

#arrange data with ascending location and descending ba
ord6 <- salary[order(salary$Location, -salary$ba),]; ord6

#arrange data with descending location and ascending ba
ord7 <- salary[order(salary$Location, decreasing = T, -salary$ba),]; ord7



### merge: when we bind multiple data frames [ having at least one COMMON columns]
#join: 2 data frames at a time
# types of join: inner, left, right, full
# 1st DF = x DF, left DF
# 2nd DF = YDF, Right DF

# inner: intersection
# outer/full : union

id <- c(1,2,3,5,7)
name <- c('A','B','C','D','E')
sal <- c(20,25,30,35,40)
emp_sal <- data.frame(id,name,sal)

id<-c(1,2,3,4,6)
bonus <- c(5,10,15,20,25)
emp_bonus <- data.frame(id, bonus)

#1.inner merge : common between emp_sal and emp_bonus
inner_merge <- merge(emp_sal, emp_bonus, by = 'id') ; inner_merge #by tells which columns are common(optional)

#2. left merge
left_merge <- merge(emp_sal, emp_bonus, by = 'id', all.x = T); left_merge

#3.right merge
right_merge <- merge(emp_sal, emp_bonus, by= 'id', all.y = T); right_merge

#4.outer merge : union / combination of both emp_sal and emp_bonus
outer_merge <- merge(emp_sal, emp_bonus, by= 'id', all = T); outer_merge




### rbind: rwo binding

id <- 1:3
name <- c('a','b','c')
emp1 <- data.frame(id, name)

id <- 4:6
name <- c('p','q','r')
emp2 <- data.frame(id, name)

emp1
emp2
emp3 <- rbind(emp1, emp2); emp3

#rules for rbind
#1. no. fo columns in all DF MUST BE SAME
#2. Column names in all DF MUST BE SAME