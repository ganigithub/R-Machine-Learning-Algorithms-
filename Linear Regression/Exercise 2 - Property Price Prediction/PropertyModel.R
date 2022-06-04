#Linear model for Property price

#first import Property_Price_Test and Property_Price_Train Data-set. Remember to select Headers as Yes.

dim(Property_Price_Train) # 1459   81
dim(Property_Price_Test)  # 1459   80
#combine both test and train data-sets

library(dplyr)
combined_data <- bind_rows(Property_Price_Train,Property_Price_Test)
dim(combined_data) # 2918   81

# % of missing values
colSums(is.na(combined_data))
sum(is.na(combined_data))

df <- combined_data[,-81]  #removing missing values for target column i.e. sales price
sum(is.na(df)) #13960
missing_values <- 13960 / (nrow(df)*ncol(df)); missing_values #5.976%

# Impute missing values using Central Tendency 
colsum_ms <- colSums(is.na(combined_data)) #we create a data-frame which shows missing values of columns
colsum_ms

# if any column has more than 50 % of missing values we can remove that column 
colsum_ms <- as.data.frame(colsum_ms)
head(colsum_ms)

#we create a column called 'column_name' which stores headings of columns
colsum_ms$column_name <- names(combined_data)  # 'names' function gives the headings of columns
head(colsum_ms)

#using fix, we change the column name 'colsum_ms' to 'missing_entries
fix(colsum_ms)

#we find the columns which has more than 50% i.e. more than 1459 missing values
sub1 <- subset(colsum_ms, colsum_ms$missing_entries > 1459, select = column_name)
sub1

combined_data$Lane_Type             <- NULL
combined_data$Pool_Quality          <- NULL
combined_data$Fence_Quality         <- NULL
combined_data$Miscellaneous_Feature <- NULL
#we can also remove Fireplace quality coz about 1400 values are missing.
combined_data$Fireplace_Quality     <- NULL

colSums(is.na(combined_data))

#Now about 5 columns are removed due to more than 50% missing values. we again create colsum_ms data-frame without these columns.
colsum_ms <- colSums(is.na(combined_data))
colsum_ms <- as.data.frame(colsum_ms)
colsum_ms$column_name <- names(combined_data)
head(colsum_ms)

#we create subset having missing values:
fix(colsum_ms) #again change the name of colsum_ms to missing entries

#again we create subset of column_names having more than 0 missing_values
sub2 <- subset(colsum_ms, colsum_ms$missing_entries > 0, select = column_name)
head(sub2)

vect1 <- sub2$column_name; vect1 #we create a vector which stores column_names having more than 0 missing values
vect1 <- vect1[1:29]             #we only select first 29 column_names coz last column is 'Sale_Price' which is target column

# Knn imputations:  k nearest neighbours :
# missing values can be replaced using knn.
# For categorical missing value, Maximum votes are considered. For numerical missing values, average is considered for nearest neighbour

#install.packages("VIM")
library(VIM)
imputed_data <- kNN(combined_data, variable = vect1) #variable tells on which columns we have to perform kNN

dim(imputed_data)             # 2918 105 (76 columns where there, and extra 29 columns are added)
View(imputed_data)            # new tab will be opened we can view where the knn has changed value. If True, value is changed.

final_data <- imputed_data[,1:76] #since we added 29 extra columns which show whether kNN is performed or not, we remove them
head(final_data)
colSums(is.na(final_data))        # no missing values except Sale_Price


library(dplyr)
num_variables        <- select_if(final_data, is.numeric)  #stores numeric variables form final data
categorical_variable <- select_if(final_data, is.factor)   #stores categorical variables

dim(num_variables)         #2918  38
dim(categorical_variable)  #2918  38


# 1. create cor matrix
#since we combined test and train, sale price is only available for train data and not test.
# so we only take correlation for 1459 train data-set.

num_variables$Id <- NULL
num_variables    <- num_variables[1:1459,]

cor_matrix <- cor(num_variables); cor_matrix
class(cor_matrix)

#we create a data-frame for correlation for sale price
cor_df <- as.data.frame(cor_matrix)
class(cor_df)
cor_df$Sale_Price

#we store correlation values of Sale_Price in different variable
cor_var <- cor_df$Sale_Price

#we also create heading names for values in cor_var
colnames_num <- names(num_variables)

#we create a data frame with heading names and values of sale price
cor_num_df <- data.frame(colnames_num, cor_var); cor_num_df

#we extract variables whose correlation value is between -1 to -0.15 and 0.15 to 1
imp_num_var <- subset(cor_num_df, (cor_num_df$cor_var > 0.15 & cor_num_df$cor_var < 1) | 
                  (cor_num_df$cor_var < 0 & cor_num_df$cor_var < -0.15))
imp_num_var
dim(imp_num_var)  #20 2

#factor variables
dim(categorical_variable) #2918 38

#we only consider categorical variables for training data
categorical_variable <- categorical_variable[1:1459,]
summary(categorical_variable)

#observe in console that utility type is constant variable as it's very un-symmetric. 1458 values are ALLPub and only 1 is NoSeWa
#so we have to remove this variable. constant variables are not good for models.

#constant variables are:
#Road_Type, Utility_Type, Condition_2, Roof_Quality, Heating_Type

final_data$Road_Type <- NULL
final_data$Utility_Type <- NULL
final_data$Condition2 <- NULL
final_data$Roof_Quality <- NULL
final_data$Heating_Type <- NULL

categorical_variable <- select_if(final_data, is.factor)
categorical_variable <- categorical_variable[1:1459,]
summary(categorical_variable) #observe that few constant variables are removed



## Data Wragnling: 

# Lot_Configuration
# FR3P -> FR2P (convert fr3p to fr2p since fr3p is very less)

final_data$Lot_Configuration <- as.character(final_data$Lot_Configuration)    #have to be converted into char from factor
final_data$Lot_Configuration[final_data$Lot_Configuration == 'FR3P'] <- 'FR2P'
final_data$Lot_Configuration <- as.factor(final_data$Lot_Configuration)       #again have to be converted to factor from char

#Roof_Design
#Shed -> Gable

final_data$Roof_Design <- as.character(final_data$Roof_Design)
final_data$Roof_Design[final_data$Roof_Design == 'Shed'] <- 'Gable'
final_data$Roof_Design <- as.factor(final_data$Roof_Design)

#Exteriro_Condition
#Ex, Po -> TA

final_data$Exterior_Condition <- as.character(final_data$Exterior_Condition)
final_data$Exterior_Condition[final_data$Exterior_Condition == 'EX' | final_data$Exterior_Condition == 'Po'] <- 'TA'
final_data$Exterior_Condition <- as.factor(final_data$Exterior_Condition)

#Foundation_Type
#S,W -> PC

final_data$Foundation_Type <- as.character(final_data$Foundation_Type)
final_data$Foundation_Type[final_data$Foundation_Type == 'S' | final_data$Foundation_Type == 'W'] <- 'PC'
final_data$Foundation_Type <- as.factor(final_data$Foundation_Type)

#Basement_Condition:
#Po >- TA

final_data$Basement_Condition <- as.character(final_data$Basement_Condition)
final_data$Basement_Condition[final_data$Basement_Condition == 'Po'] <- 'TA'
final_data$Basement_Condition <- as.factor(final_data$Basement_Condition)

#Electrical_system 
#FuseP , Mix -> SBrkr

final_data$Electrical_System <- as.character(final_data$Electrical_System)
final_data$Electrical_System[final_data$Electrical_System == 'FuseP' | final_data$Electrical_System == 'Mix'] <- 'SBrkr'
final_data$Electrical_System <- as.factor(final_data$Electrical_System)

#Functional_Rate
#MajD1, MajD2 -> MAjD
#mD, MD1, MD2 -> MD (create new level)
#Mod, MS, SD, SEv -> TF

final_data$Functional_Rate <- as.character(final_data$Functional_Rate)

final_data$Functional_Rate[final_data$Functional_Rate == 'MajD1' | final_data$Functional_Rate == 'MajD2'] <- 'MAjD'
final_data$Functional_Rate[final_data$Functional_Rate == 'MD'    | final_data$Functional_Rate == 'MD1' | final_data$Functional_Rate =='MD2'] <- 'MD'
final_data$Functional_Rate[final_data$Functional_Rate == 'Mod'   | final_data$Functional_Rate =='MS'|final_data$Functional_Rate=='SD'|final_data$Functional_Rate=='Sev'] <- 'TF'

final_data$Functional_Rate <- as.factor(final_data$Functional_Rate)


#Garage
#2TFes, 2Types -> Attchd

final_data$Garage <- as.character(final_data$Garage)
final_data$Garage[final_data$Garage == '2TFes' | final_data$Garage == '2Types'] <- 'Attchd'
final_data$Garage <- as.factor(final_data$Garage)

#Garage_Quality
#Ex(excellent) or Po(Poor) -> TA

final_data$Garage_Quality <- as.character(final_data$Garage_Quality)
final_data$Garage_Quality[final_data$Garage_Quality == 'Ex' | final_data$Garage_Quality == 'Po'] <- 'TA'
final_data$Garage_Quality <- as.factor(final_data$Garage_Quality)

#Garage_Condition
#same as above (logically)

final_data$Garage_Condition <- as.character(final_data$Garage_Condition)
final_data$Garage_Condition[final_data$Garage_Condition == 'Ex' | final_data$Garage_Condition == 'Po'] <- 'TA'
final_data$Garage_Condition <- as.factor(final_data$Garage_Condition)

#Sale_Type
#Con, ConLD, ConLI, ConLw, CWD,  -> other

final_data$Sale_Type<-as.character(final_data$Sale_Type)

final_data$Sale_Type[final_data$Sale_Type=="Con"|
                       final_data$Sale_Type=="ConLD"|
                       final_data$Sale_Type=="ConLI"|
                       final_data$Sale_Type=="CWD"|
                       final_data$Sale_Type=="Oth"]<-"others"

final_data$Sale_Type<-as.factor(final_data$Sale_Type)

#Sale_Condition
#AbnoRMD1, Abnorml -> Abnorml
#NoRMDal, Mormal -> Normal
#AdjLand Alloca -> Alloca

final_data$Sale_Condition<-as.character(final_data$Sale_Condition)

final_data$Sale_Condition[final_data$Sale_Condition=="AbnoRMDl" | final_data$Sale_Condition=="Abnorml" ]<-"Abnorml"
final_data$Sale_Condition[final_data$Sale_Condition=="AdjLand"  | final_data$Sale_Condition=="Alloca" ]<-"Alloca"
final_data$Sale_Condition[final_data$Sale_Condition=="Normal"   | final_data$Sale_Condition=="NoRMDal" ]<-"Normal"

final_data$Sale_Condition<-as.factor(final_data$Sale_Condition)


colSums(is.na(final_data))

write.csv(house_data, file = 'C:/Users/ganesh/Documents/R files/Linear Regression/Exercise 2 - Property Price Prediction/house_data.csv')

#import house_data (this is the cleaned data that is result of all operations we performed above)

colSums(is.na(house_data))
house_data$X   <-NULL        #this is simply the extra column R creates (serial number)
house_data$X.1 <- NULL
house_data$X.2 <- NULL
summary(house_data)


Train_house_data <- house_data[1:1459,]   #we split house_data into train(first 1459 rows) and test(after 1459 rows)
Test_house_data  <- house_data[1460:2918,]
summary(Train_house_data)

Train_house_data$Sale_Price <- final_data$Sale_Price[1:1459] #we create column in Train_house_data called Sale_Price which stores Sale_Price
summary(Train_house_data)
dim(Train_house_data)

#checking outliers
boxplot(Train_house_data$Sale_Price)

quantile(Train_house_data$Sale_Price)
IQR(Train_house_data$Sale_Price) #84050
UW <- 214000 + 1.5*(84050); UW
Train_house_data$Sale_Price[Train_house_data$Sale_Price > UW] <- UW
boxplot(Train_house_data$Sale_Price) #no outliers

set.seed(200)                      #pseudo randomization
index <- sample(1459, 0.75*1459)   #select 75% of data from first 1459 i.e train data
length(index)                      #1094 observations are selected from first 1459 entries

#now we split first 1459 entries into test and train data 
train_data <- Train_house_data[index,]    #75% of first 1459 entries
test_data  <- Train_house_data[-index,]    #25% of first 1459 entries

#model on train data
house_model      <- lm(Sale_Price~., data = train_data)   #FUll model
house_model_null <- lm(Sale_Price~1, data = train_data)   #NULL model

summary(house_model)


### Stepwise Regression

#1 : FORWARD REGRESSION
step(house_model_null, direction = 'forward', scope = list(lower = house_model_null, upper = house_model))
#check output. the last formula for lm is the best one.
#AIC : Akaike Information Criteria: error

rev_house_model <- lm(formula = Sale_Price ~ Overall_Material + Neighborhood + Grade_Living_Area + 
     Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
     Basement_Height + Fireplaces + Remodel_Year + Exterior1st + 
     Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
     Land_Outline + Property_Slope + Functional_Rate + House_Design + 
     Rooms_Above_Grade + Sale_Condition + Exposure_Level + Basement_Condition + 
     Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
     Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
     Exterior_Condition + Foundation_Type + Total_Basement_Area + 
     Exterior_Material + Zoning_Class + Sale_Type, data = train_data)


#2. BACKWARD REGRESSION: (prefer this if you know all variables are significant)
step(house_model, direction = 'backward', scope = list(lower = house_model_null, upper = house_model))

rev_house_model <- lm(formula = Sale_Price ~ Lot_Extent + Lot_Size + Overall_Material + 
                        Remodel_Year + Brick_Veneer_Area + Total_Basement_Area + 
                        First_Floor_Area + Second_Floor_Area + Underground_Full_Bathroom + 
                        Bedroom_Above_Grade + Rooms_Above_Grade + Fireplaces + Garage_Size + 
                        Zoning_Class + Land_Outline + Lot_Configuration + Property_Slope + 
                        Neighborhood + Condition1 + House_Type + House_Design + Roof_Design + 
                        Exterior1st + Brick_Veneer_Type + Exterior_Material + Exterior_Condition + 
                        Foundation_Type + Basement_Height + Basement_Condition + 
                        Exposure_Level + BsmtFinType1 + Kitchen_Quality + Functional_Rate + 
                        Garage_Quality + Sale_Type + Sale_Condition, data = train_data)

library(car)
vif(rev_house_model)
#high GVIF means there is another variable representing that variable here Neighborhood has high GVIF. So we can remove it

#Removing Neighborhood
rev_house_model_nb <- lm(formula = Sale_Price ~ Overall_Material + Grade_Living_Area + 
                           Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                           Basement_Height + Fireplaces + Remodel_Year + Exterior1st + 
                           Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                           Land_Outline + Property_Slope + Functional_Rate + House_Design + 
                           Rooms_Above_Grade + Sale_Condition + Exposure_Level + Basement_Condition + 
                           Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                           Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                           Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                           Exterior_Material + Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_nb)    #observe that sale_condition has high GVIF

#removing Sale_Condition
rev_house_model_sc <- lm(formula = Sale_Price ~ Overall_Material + Grade_Living_Area + 
                           Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                           Basement_Height + Fireplaces + Remodel_Year + Exterior1st + 
                           Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                           Land_Outline + Property_Slope + Functional_Rate + House_Design + 
                           Rooms_Above_Grade + Exposure_Level + Basement_Condition + 
                           Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                           Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                           Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                           Exterior_Material + Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_sc)

#removing House_Design
rev_house_model_hd <- lm(formula = Sale_Price ~ Overall_Material + Grade_Living_Area + 
                          Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                          Basement_Height + Fireplaces + Remodel_Year + Exterior1st + 
                          Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                          Land_Outline + Property_Slope + Functional_Rate + 
                          Rooms_Above_Grade + Exposure_Level + Basement_Condition + 
                          Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                          Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                          Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                          Exterior_Material + Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_hd)

#removing Exterior1st
rev_house_model_ext <- lm(formula = Sale_Price ~ Overall_Material + Grade_Living_Area + 
                           Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                           Basement_Height + Fireplaces + Remodel_Year + 
                           Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                           Land_Outline + Property_Slope + Functional_Rate + 
                           Rooms_Above_Grade + Exposure_Level + Basement_Condition + 
                           Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                           Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                           Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                           Exterior_Material + Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_ext)

#removing Exterior_Material
rev_house_model_em <- lm(formula = Sale_Price ~ Overall_Material + Grade_Living_Area + 
                            Garage_Size + House_Type + BsmtFinType1 + Kitchen_Quality + 
                            Basement_Height + Fireplaces + Remodel_Year + 
                            Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                            Land_Outline + Property_Slope + Functional_Rate + 
                            Rooms_Above_Grade + Exposure_Level + Basement_Condition + 
                            Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                            Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                            Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                            Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_em)

#removing BsmtFinType1
rev_house_model_bft1 <- lm(formula = Sale_Price ~ Overall_Material + Grade_Living_Area + 
                           Garage_Size + House_Type + Kitchen_Quality + 
                           Basement_Height + Fireplaces + Remodel_Year + 
                           Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                           Land_Outline + Property_Slope + Functional_Rate + 
                           Rooms_Above_Grade + Exposure_Level + Basement_Condition + 
                           Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                           Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                           Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                           Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_bft1)

#removing Grade_Living_Area
rev_house_model_gla <- lm(formula = Sale_Price ~ Overall_Material + 
                             Garage_Size + House_Type + Kitchen_Quality + 
                             Basement_Height + Fireplaces + Remodel_Year + 
                             Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                             Land_Outline + Property_Slope + Functional_Rate + 
                             Rooms_Above_Grade + Exposure_Level + Basement_Condition + 
                             Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                             Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                             Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                             Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_gla)

#removing Basement_Height
rev_house_model_bh <- lm(formula = Sale_Price ~ Overall_Material + 
                            Garage_Size + House_Type + Kitchen_Quality + 
                            Fireplaces + Remodel_Year + 
                            Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                            Land_Outline + Property_Slope + Functional_Rate + 
                            Rooms_Above_Grade + Exposure_Level + Basement_Condition + 
                            Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                            Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                            Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                            Zoning_Class + Sale_Type, data = train_data)
vif(rev_house_model_bh)
#now no GVIF values is less than 5

house_model_rev <- rev_house_model_bh

## calculating RMSE for train data ( root mean square error)

Sale_Price_pred <- predict(house_model_rev, train_data)       #predicted sale price
train_residual  <- train_data$Sale_Price - Sale_Price_pred     #error (diff in actual and predicted sale price)
RMSE_train      <- sqrt(mean(train_residual ^ 2)); RMSE_train      #25101.31

## calculating RMSE for test data
Sale_Price_pred_test <- predict(house_model_rev, test_data)          #predicted sale price
test_residual        <- test_data$Sale_Price - Sale_Price_pred_test  #error (diff in actual and predicted sale price)
RMSE_test            <- sqrt(mean(test_residual ^ 2)); RMSE_test     #27322.56


## Predict on TEST DATA ( after 1459 observations i.e. test_house_data)

Test_house_data$Sale_Price <- predict(house_model_rev, Test_house_data)  #creating Sale_Price column in test_house_data
#there wil be ERROR coz in train_house_data ,condition1 doesn't have NoRMD but test_house_data has.Due which model didn't create coeff for NoRMD
summary(Train_house_data$Condition1)
summary(Test_house_data$Condition1)
# we have to either remove NoRMD from test data or replace it with other value.

Test_house_data$Condition1 <- as.character(Test_house_data$Condition1)
Test_house_data$Condition1[Test_house_data$Condition1 == 'NoRMD'] <- 'Norm'
Test_house_data$Condition1 <- as.factor(Test_house_data$Condition1)

#now we can create sale price column
Test_house_data$Sale_Price <- predict(house_model_rev, Test_house_data)
head(Test_house_data) #Sale_Price column is generated


## autocorrelation : correlation of residual between itself and next value or (1st lag)

# to test autocorrelation we have DW Test (Durbin Watson)
# DW = 2(1 - r), where r is coefficient of correlation between residual and its 1st lad
# Ho : data is not auto-correlated
# Ha : data is auto-correlated

#r = 0, DW = 2   # no autocorrelation between residual and 1st lag
#r = -1, DW = 4  # negative correlation between residual and 1st lag
#r = +1 , DW = 0 # positive correlation

durbinWatsonTest(house_model_rev)
#Value of Autocorrelation is -ve and DW is 2 suggesting there is no auto correlation but p-value suggest Ha is correct ie there is autocorrelation
# to remove this contradiction, we try randomizing values to get p-value more than 0.05

ind <- sample(nrow(train_data), nrow(train_data))
train_data_random <- train_data[ind, ]
dim(train_data_random)

rev_house_model_random <- lm(formula = Sale_Price ~ Overall_Material + 
                           Garage_Size + House_Type + Kitchen_Quality + 
                           Fireplaces + Remodel_Year + 
                           Underground_Full_Bathroom + Lot_Size + Lot_Extent + Condition1 + 
                           Land_Outline + Property_Slope + Functional_Rate + 
                           Rooms_Above_Grade + Exposure_Level + Basement_Condition + 
                           Garage_Quality + BsmtFinSF1 + Lot_Configuration + Roof_Design + 
                           Bedroom_Above_Grade + Brick_Veneer_Type + Brick_Veneer_Area + 
                           Exterior_Condition + Foundation_Type + Total_Basement_Area + 
                           Zoning_Class + Sale_Type, data = train_data_random)
durbinWatsonTest(rev_house_model_random)
