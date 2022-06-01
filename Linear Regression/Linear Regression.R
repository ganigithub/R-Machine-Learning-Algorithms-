### Predictive analysis


#supervised learning: input and output
# unsupervised learning: input

#supervised : regression and classification types

#regression: when outcome is continuous  eg  how many runs player will score
#classification: when outcome is categorical eg win or loose

# Linear Regression : descriptive + predective
# Linear regression: simple LR / Multiple LR

# SLR: one independent variable, one dependent variable (price wrt sq_ft)
# MLR: multiple ind var, one dep var (price wrt sq_ft and distance from station)

# eq of simple linear regression: y = mx + c: y = b0 + b1 x1 + error
# eq of multiple

# linear regression: y = mx + c: y = b0 + b1 x1 + b2 x2 + ... + bn xn + error

## import jpi file as proficiency

proficiency$X<-NULL
proficiency$X.1<-NULL
proficiency$X.2<-NULL
proficiency$empid<-NULL

#correlation of all numeric variables
library(corrplot)

#generate cor matrix
cor_matrix <- cor(proficiency)
cor_matrix
corrplot(cor_matrix, method='circle', type='lower')

# using boxplot check IQR, UW and LW
#check for any outliers

boxplot(proficiency$jpi)
IQR <- IQR(proficiency$jpi)  #12.73
quantile(proficiency$jpi)
UW <- 53.92 + 1.5*(IQR); UW  #73.015
LW <- 41.19 - 1.5*(IQR); LW  #22.095

#check for missing values
colSums(is.na(proficiency))

# Building Linear model (MLR)

jpi_lm <- lm(jpi~written+language+tech+gk, data = proficiency) #linearmodel(dep var ~ ind variables)

# jpi_lm <- lm(jpi~., data = proficiency) # this will consider all variables as ind var except dep var
jpi_lm

#from output:
# b0 / c = -54.28
# b1 / m1 = 0.32356 #for written
# b2 / m2 = 0.03337 #for lanugage
# b3 / m3 = 1.09547  #for tech
# b4 / m4 = 0.53683  #for gk

#when written value increases by 1 unit; jpi value increases by 0.32356 units; provided all other var are constant

#i.e if jpi = written = language = tech = gk = 40
# if written = 40+1 = 41, jpi = 40 + 0.32356 = 40.32356

#assume if lang is increased by 10 units what will be jpi
# lang = 40 + 10 = 50 , jpi =  40 + (10*0.03337) = 40.3337

# assume if tech is increased by 10 units what is jpi? ; jpi = 40 + (10*1.09547) = 50.9547

head(proficiency,1)

# y = 45.52, x1 = 43.83, x2 = 55.92, x3 = 51.82, x4 = 43.58
y_pred1 <- -54.28225 + ((0.32356*43.83)+(0.0337*55.92)+(1.09547*51.92)+(0.53683*43.58)) ; y_pred1 # 42.05574

#check pricted values for first row
y_predicted <- predict(jpi_lm, proficiency)
head(y_predicted)

#get residual or error for first row
y_res <- proficiency$jpi - y_predicted
head(y_res)

sum(y_res) #sum of all residuals

#create a df of 
df1 <-data.frame(proficiency)

#goodness of fit for given model
quantile(y_res)

summary(jpi_lm)
# F-statistic: 49.81 on 4 and 28 DF,  p-value: 2.467e-12 means; pvalue is Golbal hypothesis
#H0 : nyll model and jpilm are similar
#H1 : null model and jpi_lm are not similar where; 
# null model : model without any ind var i.e all variables are insignificant

#creating a null model
lm_jpi_null <- lm(jpi~1, data = proficiency)
#to compare models we have to use anova. aov is for comparing 
anova(lm_jpi_null, jpi_lm)
#p < 0.05; reject H0

#since language is not significant; we can remove it
rev_lm_jpi<-lm(jpi~.-(language), data = proficiency)
summary(rev_lm_jpi)

#adjusted R-squared: 0.863 > 0.6: model is fit to be used on any out of sample data
written <- c(45,50,52,54,60)
language <- c(50,45,65,55,54)
tech <- c(40,45,55,60,59)
gk <- c(50,62,54,32,50)

df_new <- data.frame(written, language, tech,gk); df_new

df_new$jpi <- predict(rev_lm_jpi, df_new); df_new


## multicolinearity
jpi_lm
#VIF: variance inflation factor : 1 / (1-R^2) : if VIF > 5, NO multicolinearity. If VIF<5, multicolinearity exists

#install.packages("car") #Companion to Applied Regression
library(car)
vif(jpi_lm) #all variables are < 5 so NO multicolinearity
vif(rev_lm_jpi)

dim(Property_Price_Test)
dim(Property_Price_Train)