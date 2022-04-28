## 1.	Import automobile dataset

auto <- read.csv(file = 'C:/Users/ganesh/Documents/R files/Automobile_data.csv', stringsAsFactors = T)

## 2.	Replace "?"  with NA

auto[auto=='?'] <- NA
sum(is.na(auto))      #59 NA values
colSums(is.na(auto))
59 / (nrow(auto) * ncol(auto))   # 0.011 % missing values

## 3.	Impute NA with Central tendency 

summary(auto)

#we convert normalized losses column from character to numeric and replace NA with mean
auto$normalized.losses = as.numeric(as.character(auto$normalized.losses))
mean(auto$normalized.losses, na.rm = T)

auto$normalized.losses[is.na(auto$normalized.losses)] <- mean(auto$normalized.losses, na.rm = T)

#we replace NA with mode for No of doors columns
summary(auto$num.of.doors)
auto$num.of.doors[is.na(auto$num.of.doors)] <- 'four'

#we convert bore column from char to numeric and replace NA with mean
auto$bore = as.numeric(as.character(auto$bore))
mean(auto$bore, na.rm = T)

auto$bore[is.na(auto$bore)] <- mean(auto$bore, na.rm = T)

#we convert stroke to numeric and replace NA with median
auto$stroke = as.numeric(as.character(auto$stroke))
median(auto$stroke,na.rm = T)

auto$stroke[is.na(auto$stroke)] <- median(auto$stroke, na.rm = T)

#we convert price column to numeric and replace NA with mean

auto$price = as.numeric(as.character(auto$price))

auto$price[is.na(auto$price)] <- mean(auto$price, na.rm = T)

#we convert horsepower to numeric and replace with mean
auto$horsepower = as.numeric(as.character(auto$horsepower))

auto$horsepower[is.na(auto$horsepower)] <- mean(auto$horsepower, na.rm = T)

#we convert peak.rpm to numeric and replace with mean
auto$peak.rpm = as.numeric(as.character(auto$peak.rpm))
auto$peak.rpm[is.na(auto$peak.rpm)] <- mean(auto$peak.rpm, na.rm = T)

sum(is.na(auto)) #All NA values replaced with mean / median / mode

summary(auto)


## 4.	Check boxplot of price variable 
boxplot(auto$price)

## 5.	Replace ourtlies of price variable with Upper Whisker or with Lower Whisker 
summary(auto$price)
IQR = IQR(auto$price)
UW = 16500 + 1.5 * IQR

auto$price[auto$price > UW] <- UW  #since outliers are above UW, we replace outliers with UW
boxplot(auto$price)

## 6.	What is average price with respect to number of cylinders?
price <- aggregate(price ~ num.of.cylinders, data = auto, FUN = mean); price

## 7.	What is median of peak - rpm with respect to fuel system ?
peak_rpm <- aggregate(peak.rpm ~ fuel.system, data = auto, FUN = median); peak_rpm

## 8.	Find variance in horsepower with respect to gas type and engine type ?
horsepower <- aggregate(horsepower ~ fuel.type + engine.type, data = auto, FUN = var); horsepower

## 9.	Create subset of sedan cars 
sedan <- subset(auto, auto$body.style == 'sedan');
head(sedan)

## 10.	Create subset of cars with gas as fuel type and hatchback /sedan type
s1 <- subset(auto, auto$fuel.type == 'gas' & (auto$body.style =='hatckback' | auto$body.style =='sedan'))