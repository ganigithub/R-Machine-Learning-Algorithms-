
colSums(is.na(Telecom_CustDemo))
colSums(is.na(Telecom_WeeklyData))

##find total usage of each customer wrt calls
total_usage <- aggregate(Calls ~ CustID, data= Telecom_WeeklyData, FUN = sum)
head(total_usage)

##find total usage of calls, mins and amt wrt custid
total_usage <- aggregate(cbind(Calls,Amt, Minutes) ~ CustID, data = Telecom_WeeklyData, FUN= sum)
head(aggr)

##fetch tatal Amt for each customer; CustID, amount and active status

#we need to merge to datasets coz Active, CustID are in different dataset 
merged_data <- merge(Telecom_CustDemo, total_usage)
head(merged_data)

sub1 <- merged_data[,c('CustID', 'Amt', 'Active')]
head(sub1)

##find customers having total usage amount > median amount

sub2 <- subset(merged_data, merged_data$Amt > median(merged_data$Amt))
head(sub2)

##create column age_group such that if age < median(age) insert 'cat1 else insert 'cat2' in age_group

# using ifelse: ifelse(test, if True, if False)

merged_data$Age_Group <- ifelse(merged_data$Age < median(merged_data$Age), 'cat1','cat2')
head(merged_data)

# create column Age_Group_1 such that if 18<=age<=25 its cat1
#if age > 25 and <= 35  its cat2
# if age > 35 and <= 45 its cat3
# else cat4

merged_data$Age_Group_1 <- ifelse(merged_data$Age <= 25, 'cat1',
                                  ifelse(merged_data$Age <= 35, 'cat2',
                                         ifelse(merged_data$Age <= 45,'cat3','cat4')))

#using cut function: cut('column in which we want to break', breaks, labels)
merged_data$Age_Group_Cut <- cut(merged_data$Age, breaks = c(0,25,35,45,Inf), labels=c('cat1','cat2','cat3','cat4')
# in breaks

#find var in total amount for each age group
var_amt <- aggregate(Amt ~ Age_Group_1 , data= merged_data, FUN= var)
var_amt

#find top5 and bottom 5 customers w.r.t usage in terms of total calls
#display their id, age, calls and active status

bottom_calls <- merged_data[order(merged_data$Calls),]
head(bottom_calls)
bottom_5 <- bottom_calls[1:5, c('CustID','Age','Calls','Active')]; bottom_5


top_calls <- merged_data[order(merged_data$Calls, decreasing = T),]
head(top_calls)
top_5 <- top_calls[1:5, c('CustID','Age','Calls','Active')]; top_5


#find top5 and bottom5 customers using Amt variable for each gender


bottom_cust<-total_usage3[order(total_usage3$Amt),]
head(bottom_cust)
bottom_cust5<-bottom_cust[1:5,c(1,3,6,7)]
sub_bottom<-subset(bottom_cust5,bottom_cust5$Gender=="M")
sub_bottom

sorted_data<-total_usage3[order(total_usage3$Gender,total_usage3$Amt),]
least_Amt_F<-head(sorted_data,5)
sorted_data_F<-subset(sorted_data,sorted_data$Gender=="F")
sorted_data_M<-subset(sorted_data,sorted_data$Gender=="M")
tail(sorted_data_F,5)
tail(sorted_data_M,5)


## correlation: relation between two numerical variables.
# value is bet -1 to +1.
# if value is between 0.7 to 1 or -0.7 to -1 its strongly +ve or -ve correlated
# if value is between 0.4 to 0.7 or -0.4 to -0.7 its moderately +ve or -ve correlated
# if value is between 0.2 to 0.4 or -0.2 to -0.4 its weakly +ve or -ve correlated
# if value is between 0.2 to 0 or -0.2 to 0 its strongly +ve or -ve correlated

# check if age has correlation with minutes
cor(merged_data$Age, merged_data$Minutes) #-0.04 No correlation
cor(merged_data$Minutes, merged_data$Amt) #+0.84 strongly correlated

#find cor of all numerical values among themselves
num_var <- merged_data[,c('Age','Calls','Amt','Minutes')]
cor_mat <- cor(num_var); cor_mat

#corrplot: we can get heat-map for large no correlations

# install.packages('corrplot')  #once packages is installed, comment it so that it doesn't run again
library(corrplot)  #call the corrplot to use it just once

corrplot(cor_mat) #look at heat-map.
corrplot(cor_mat, method = 'square', type='lower')
