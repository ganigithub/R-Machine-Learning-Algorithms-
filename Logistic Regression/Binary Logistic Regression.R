## Binary Logistic regression

# dependent variable is categorical or nominal or only two levels (T/F, Y/S)
# output is probability (between 0 - 1) of event
# spline distribution / Binomial Distribution
# logistic : log transform (link) [ log(p / 1-p) ]
# probability , odds, odds ratio are the statistics on which model quality is determined

# odds in favor = p / 1-p   = p / q (occurrence / non-occurrence)
# odds against  = 1 - p / p = q / p (non-occurrence / occurrence)
# odds ratio    = event was success / failure / neutral

# prob of getting head   : p(head)    = 0.5
# odds of getting head   : odds(head) = p/q = 0.5 / 0.5 = 1    (odds regular)
# loaded coin : if p(0.7): odds(head) = p/q = 0.7 / 0.3 = 2.3  (odds experimental)

# odds ratio  : odds_Experimental / odds_regular = 2.3 / 1 = 2.3 ~ 2
# means likelihood of getting head increases twice when we use loaded coin w.r.t the non loaded coin


# example voting:
#             R exp (100)(campaign)         R control(100) (no campaign)
# voted           80                                   45
# not voted       20                                   55

# odds (exp)     : 80/20 = 4
# odds (control) : 45/55 = 9/11
# odds ratio  = odds(exp) / odds (control) = 4 * 11 / 9 = 44 / 9 ~ 5
# means 5 times more people are voting in experimental than control region


# example vaccine: (determine whether vaccination was successful or not using odds ratio)
#             R exp (100)(vaccine)   R control (100) (no vaccine)
# ill            30                         45
# not ill        70                         55

# odds exp     : 30 / 70 = 3 / 7
# odds control : 45 / 55 = 9 / 11
# odds ratio   : 3 / 7  *  9 / 11  = 33/63 = 0.52
# likelihood of people getting ill is half in exp region than in control region i.e vaccination was successful

# log(p / 1-p) = link transformation or logit transformation



### import BANK.LOAN as BANK_LOAN

# not required for regression
BANK_LOAN$SN <- NULL

# check for the correct format of dataset
str(BANK_LOAN)

# convert age and defaulter to format
BANK_LOAN$AGE       <- as.factor(BANK_LOAN$AGE)
BANK_LOAN$DEFAULTER <- as.factor(BANK_LOAN$DEFAULTER)
str(BANK_LOAN)

# build logistic regression

# glm = genralized linear model
bank_glm <- glm(DEFAULTER~. , data = BANK_LOAN, family = 'binomial')
bank_glm

# observe output. Address and employ are in inverse relation with being defaulter or not
# NULL model: no variable is considered, Residual: variables are considered
# Null deviance should be more than Residual deviance. If they are similar then no variables are significant. AIC is overall error

summary(bank_glm)
# z-value,(Dispersion parameter for binomial family taken to be 1), Number of fisher scoring iterations: 6 ???

## stepwise regression

null_bank_model <- glm(DEFAULTER~1 , data = BANK_LOAN, family = 'binomial')
step (null_bank_model, direction = 'forward', scope = list(lower = null_bank_model, upper = bank_glm))

# copy the last call:
bank_glm_rev <- glm(formula = DEFAULTER ~ DEBTINC + EMPLOY + CREDDEBT + ADDRESS, 
               family = "binomial", data = BANK_LOAN)

# now we find probability

pred_prob <- predict(bank_glm_rev, BANK_LOAN, type = 'response') #for binary logistic we need to give type = 'response'
head(pred_prob)  #probabilities of being defaulter or not

# we get output in form of probability. we need to convert it to category
# if threshold > 0.5 = 1 or else 0
pred_defaulter <- ifelse(pred_prob > 0.5 , '1','0')
str(pred_defaulter)  #its in character. we need in factor
pred_defaulter <- as.factor(pred_defaulter)
summary(pred_defaulter)

#to check the correctness;
df1 <- data.frame(actual = BANK_LOAN$DEFAULTER , predicted = pred_defaulter)
head(df1)

table(actual = df1$actual, predicted = df1$predicted)

# accuracy = TP + TN / Total data (correctly predicted out of total)
accuracy <- (92 + 478) / 700 ; accuracy

# misclassification = FP + FN / Total data
mc = (39 + 91) / 700 ; mc

# sensivity : recall_1 = accuracy of 1 : true positive rate
sensitivity = 92 /(91 + 92); sensitivity  # TP / TP+FN
summary(BANK_LOAN)

# specificity : recall_0 : accuracy of 0 (how many 0's are correctly predicted from actual 0) = TN / TN+FP : true negative rate
specificity  = 478 / (478+39); specificity

# precision: how many 1 are correctly predicted out of total predicted 1 = TP / TP+FP
precision = 92 / (92+39); precision

# prevalance: percent of 1 in total data; actual ones out of total data = TP + FN / total
prevalance = (92+91) / 700; prevalance  #0's accuracy is good coz prevalance is less. if more then 1's accuracy is more.

## ROCR (receiver operational characteristic curve)
#install.packages('ROCR')
library(ROCR)

# to check accurate threshold;

# if threshold is 0.1, sensitivity = high, specificity = low
# if threshold is 0.9, sensitivity = low, specificity = high
# best possible accuracy, sensitivity and specificity > 60%
# in this case sensitivity is low so we need threshold to be reduced. required threshold for given Q will be < 0.5

#prediction function : pred_probability, actual value of dependent variable
#performance function: predicted, tpr, tnr
#plot y = sensitivity , x = specificity

pred <- prediction(pred_prob, BANK_LOAN$DEFAULTER)
perf <- performance(pred , 'tpr', 'tnr')
plot(perf, colorize = T, print.cutoffs.at = seq(0.1, 1, 0.1))

#now we reduce the threshold to 0.3
pred_defaulter_0.3 <- ifelse(pred_prob > 0.3 , '1','0')
pred_defaulter <- as.factor(pred_defaulter)

table(actual=BANK_LOAN$DEFAULTER, predicted = pred_defaulter_0.3)

accuracy    = (415 + 138) / 700    ; accuracy
mc          = (45 + 102)  / 700    ; mc
sensitivity = 138 / (138 + 45)     ; sensitivity
specificity = 415 / (415 + 102)    ; specificity
precision   = 138 / (138 + 102)    ; precision
prevalance  = (45 + 138) / 700     ; prevalance


### using uniform sample : to reduce false results

# up-sampling   : process of inserting zero-valued samples between original samples to increase the sampling rate.
# down-sampling : opposite of up-sampling

## up-sampling

# install.packages('caret') #classification and regression training
library(caret)
summary(BANK_LOAN$DEFAULTER)

##upsampling
upsample_bank_data <- upSample(BANK_LOAN, BANK_LOAN$DEFAULTER)
dim(upsample_bank_data)
summary(upsample_bank_data)

upsample_bank_data$Class <- NULL #this column is similar to defaulter so remove it
summary(upsample_bank_data$DEFAULTER)  #note that no of obs for 0 and 1 is same. 1's are increased to 517

##downsampling
downsample_bank_data <- downSample(BANK_LOAN, BANK_LOAN$DEFAULTER)
dim(downsample_bank_data)

downsample_bank_data$Class <- NULL
summary(downsample_bank_data$DEFAULTER)  #note that 0's are reduced to 183


#1. Up-sampled data:

null_sampled <- glm(DEFAULTER~1, data = upsample_bank_data, family = "binomial")
full_sampled <- glm(DEFAULTER~., data = upsample_bank_data, family = "binomial")
step(null_sampled, direction = "forward", scope = list(lower=null_sampled, upper=full_sampled))

#copy the final call in output:
bank_model_up <- glm(formula = DEFAULTER ~ AGE + EMPLOY + ADDRESS + DEBTINC + 
                     CREDDEBT + OTHDEBT, family = "binomial", data = upsample_bank_data)

# generate probability
pred_prob_up <- predict(bank_model_up, upsample_bank_data, type = "response")
# decide threshold : 50% (since we uniformed, threshold must be 50%)
pred_defaulter_up <- ifelse(pred_prob_up >= 0.5,"1","0")
pred_defaulter_up <- as.factor(pred_defaulter_up)
table(actual = upsample_bank_data$DEFAULTER, predicted = pred_defaulter_up)


#2. down-sampled data:

null_downsampled <- glm(DEFAULTER~1, data = downsample_bank_data, family = "binomial")
full_downsampled <- glm(DEFAULTER~. ,data = downsample_bank_data, family = "binomial")
step(null_downsampled,direction = "forward", scope = list(lower=null_downsampled,upper=full_downsampled))

#copy the final call in output:
bank_model_down <- glm(formula = DEFAULTER ~ DEBTINC + EMPLOY + CREDDEBT + ADDRESS + 
                       AGE, family = "binomial", data = downsample_bank_data)

# generate probability
pred_prob_down <- predict(bank_model_down, downsample_bank_data, type = "response")

# decide threshold : 50%   (since we uniformed, threshold must be 50%)
pred_defaulter_down <- ifelse(pred_prob_down >= 0.5,"1","0")
pred_defaulter_down <- as.factor(pred_defaulter_down)
table(actual = downsample_bank_data$DEFAULTER, predicted = pred_defaulter_down)