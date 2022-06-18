### HYPOTHESIS TESTING

##1. ONE SAMPLE t-test

# Ho : avg time taken = 90 mins,i.e. mu = 90  -> NULL STATEMENT
# Ha : avg time > 90 mins, i.e mu > 90      -> ALTERNATIVE STATEMENT

t.test(one_sample$Time, alternative = 'greater', mu = 90)
# alternative = greater / less / two.sided. Since we are testing for greater, we use greater.
#p-value = 0.04074

# p-value = prob of getting type-1 error if null hypothesis is rejected. i.e False Positives
# type-1 error: False Positives: Declaring data is more than 90 even though its NOT

# if p-value < 0.05 (5%) it is SAFE TO REJECT Ho. i.e. null hypothesis
# if p-value > 0.05; it is NOT SAFE TO REJECT Ho. means we accept Ho

# p value = 0.04:
#prob of getting type1 error if we reject Ho is (4%) 0.04 < 0.05 (los: level of significance). we reject Ho

# p-value = 0.8
#prob of making type1 error if we reject Ho is 80%; it is not safe to reject Ho. so we accept Ho

#p= 0.01
#prob of making type1 error is 1% if we reject Ho. safe to reject Ho

##simply, if p>0.05, we accept Ho. if not we reject Ho

head(diabetes)

# Ho : avg glucose = 135. mu = 135
# Ha : avg glucose != 135. mu != 135

t.test(diabetes$Glucose, mu=135, alternative = 'two.sided')
# when we simply need to check equality we use 'two.sided'
#here p-value is very less than 0.05, its safe to reject Ho value. i.e avg value is not 135

t.test(diabetes$Glucose, mu=120, alternative = 'two.sided')
#here p-value is more than 0.05, its not safe to reject. So we accept that avg glucose is about 120

#check if avg value of BP = 60
t.test(diabetes$BloodPressure, mu=60, alternative='two.sided')   #mean not equal to 60



##2. Two Sample t-test. also called independent sample t-test

#check if avg of two samples is similar or not
# 2 groups: 1 factor variable: with 2 levels and 1 numeric variable
# salary of Male employees with Female employees
# here num variable is salary and factor is gender(M/F)

#Ho: time required to complete MIS report by gr1 and gr2 is similar
    # or difference in time taken by grp1 and grp2 is 0

#Ha: opposite of null hypothesis

# this is 'wide data' since each factor has different columns eg. gr1 and gr2

t.test(independent_sample$time_g1, independent_sample$time_g2)
# We accept Ho. i.e time required to complete report by both groups is similar.

#check avg glucose for diabetic and non-diabetic is similar
#diabetic data is 'long data' since all factor is in single column e.g diabetic or not is in same column
str(diabetes$Outcome)
diabetes$Outcome <- as.factor(diabetes$Outcome) #outcome has to be converted to factor from int

t.test(diabetes$Glucose ~ diabetes$Outcome) #outcome is grouping factor with exactly 2 levels (1 or 0)
# we can only compare means by this.
# result says we accept Alternative Hypothesis. i.e there is diff in glucose levels of diabetic and non-diabetic

# t.test(independent_sample$time_g1 ~ independent_sample$time_g2)
# above test will not work since there must be exactly 2 levels for grouping factor



##3. Dependent sample t-test / Paired t-test
#The paired t-test is the standard method for comparing means of paired samples.

#two samples of same entity taken at different interval of time (before/after)

#Ho: time taken to complete report before and after training is similar. i.e no difference in means
#Ha != Ho

t.test(paired$time_before, paired$time_after, paired = T)
#we reject Ho. Means there is difference in time to create report after training


### test of correlation : tells whether value of correlation is significant or not

# Ho: value of corr between aptitude and job proficiency is 0 i.e. no correlation
# Ha: significant correlation between aptitude and job proficiency

cor.test(jobproficiency$aptitude, jobproficiency$job_prof)

#youtube  : brandon foltz
#blogs : analytics vidya blogs



### F test

# Ho : variance1 / variance 2 = 1
# Ha != ho

var.test(F_Test$time ~ F_Test$group)
# Two samples are group1 and group2. We are comparing variance in time for both groups
# see output : p is > 0.05 so its not safe to reject null hypothesis. thus we accept Ho
# F is ratio of variances of two samples. 
# so we can say ratio of variances is mostly close to 1 i.e experience does not impact speed of generating report



### ANOVA test:Analysis of Variance: can check variance and mean together. only works for long data

## one way anova:

#Ho:variance of all categories is same
#Ha != Ho

one_way_anova <- aov(satindex~dept, data = one_way_anova)
summary(one_way_anova)
# aov is for comparing data: we are comparing satisfaction index for each department
#see output Pr(>F): p-value w.r.t F value

#import heart data

heart$cp <- as.factor(heart$cp)
chol <- aov(chol~cp , data = heart)
summary(chol)
# Ho = chol is similar across cp(chest pain)
# chol is similar across all CP. means chol level does not help to find severity of chest pain

trestbps <- aov(trestbps~cp , data = heart)
summary(trestbps)
#since we reject Ho, trestbps is not similar across all cp.

## two way anova: two factor variables and one numerical variable
# implementing one way anova individually for two factors

#without interaction
two_way <- aov(satindex ~ dept + exp, data = two_way_anova)
summary(two_way)
# p>0.05, thus satindex is same across all departments and satindex is same across all experiences

#with interaction
two_way_intr <- aov(satindex ~ dept + exp + dept*exp, data=two_way_anova)
summary(two_way_intr)
# since p>0.05, satindex is same across all interactions of dept and exp (we accept null)

# check if if trestbps changes with the change in cp and sex


trestbps <- aov(trestbps ~ cp + sex + cp*sex, data = heart)
summary(trestbps)

# check age differs significantly with change in cp and target

fix(heart)

heart$cp <- as.factor(heart$cp)
heart$target <- as.factor(heart$target)
age <- aov(age ~ cp + target + cp*target, data = heart)
summary(age)
