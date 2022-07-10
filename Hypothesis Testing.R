### HYPOTHESIS TESTING

# ho : avg time taken = 90 mins
# h1 : avg time > 90 mins

t.test(one_sample$Time, alternative = 'greater', mu = 90)
# alternative = greater / less / tow.sided
# p-value = 0.04074

# if p-value < 0.05 (5%) it is SAFE TO REJECT ho. i.e. null hypothesis
# if p-value > 0.05; it is NOT SAFE TO REJECT ho. mean we accept ho

# if p-value = prob of getting type 1 error if null hypothesis is rejected
# type 1 error: rejecting ho | ho is TRUE

# p value = 0.04:
#prob of getting type1 error if we reject ho is (40%) 0.04 < 0.05 (los: level of significance )

# p-value = 0.8
# prob of making type1 error if we reject ho is 80%; it is not safe to reject ho
# so we accept ho

#p-value = 0.1
#prob of making type1 error is 10%; we reject ho
#so we accept ho

#p= 0.01
#prob of making type1 error is 1% if we reject ho
#safe to reject ho

#simply, if p>0.05, we accept ho if not we reject ho

head(diabetes)

#ho : avg glucose = 135
# h1: avg glucose != 135

t.test(diabetes$Glucose, mu=135, alternative = 'two.sided')
#here p-value is very less than 0.05, its safe to reject ho value

t.test(diabetes$Glucose, mu=120, alternative = 'two.sided')
#here p-value is more than 0.05, its not safe to reject

#check if avg alue of BP = 60
t.test(diabetes$BloodPressure, mu=60, alternative='two.sided')   #mean not equal to 69


## two sample t-test also called independent sample t-test

#check if avg of two samples is similar or not
# 2 groups: 1 factor variable: with 2 levels and 1 numeric variable
# salary of Male employees with Female employees
# here num variable is salary and factor is gender(M/F)

#ho: time required to complete MIS report by gr1 and gr2 is similar
    # or difference in time taken by grp1 and grp2 is 0

#h1: opposite of ho null hypothesis

# this is 'wide data' since each factor has different columns eg. gr1 and gr2

t.test(independent_sample$time_g1, independent_sample$time_g2)
#thus difference in mean is not equal to 0

#check avg glucose for diab and nondiabetic is similar
#diabetic data is 'long data' since all factor is in single column e.g diabetic or not is in same column
str(diabetes$Outcome)
diabetes$Outcome <- as.factor(diabetes$Outcome) #outcome has to be converted to factor from int

t.test(diabetes$Glucose ~ diabetes$Outcome)
#we can only compare means by this.


## dependent sample ttest

#two samples of same entity taken at different interval of time (before/after)

#ho: time taken to complete report before and after training is similiar
#h1 != ho

t.test(paired$time_before, paired$time_after, paired = T)


### test of correlation : tells whether value of correlation is significant or not

#ho: value of corr between aptitide and jobproficiency is 0 ie. no correlation
# h1: significant correlation between aptitide and jobprof

cor.test(jobproficiency$aptitude, jobproficiency$job_prof)

#youtube  : brandon foltz
#blogs : analytics vidya blogs



### F test

# ho : variance1 / variance 2 = 1
# h1 != ho

var.test(F_Test$time ~ F_Test$group)
# Two samples are group1 and group2. We are comparing variance in time for both groups
# see output : p is > 0.05 so its not safe to reject null hypothesis. thus we accept ho
# F is ratio of variances of two samples. 
# so we can say ratio of variances is mostly equal to or close to 1 i.e experience does not impact speed of generating MIS report



### ANOVA test: can check variance and mean together. only works for long data

## one way anova:

one_way_anova <- aov(satindex~dept, data = one_way_anova)
summary(one_way_anova)
# aov is for comaparing data: we are comparing satifaction index for each department
#see output Pr(>F): p-value w.r.t F value

#import heart data

heart$cp <- as.factor(heart$cp)
chol <- aov(chol~cp , data = heart)
summary(chol)
# ho = chol is similar across cp(chest pain)
#chol is similar across all CP. means chol level does not helps to find severiety of chest pain

trestbps <- aov(trestbps~cp , data = heart)
summary(trestbps)


## two way anova: two facotr variables and one numerical variable
# implementing one way anova individually for two factors

#without interaction
two_way <- aov(satindex ~ dept + exp, data = two_way_anova)
summary(two_way)
# p>0.05, thus satindex is same across all departments and satindex is same across all experiences

#with iteraction
two_way_intr <- aov(satindex ~ dept + exp + dept*exp, data=two_way_anova)
summary(two_way_intr)
# since p>0.05, satindex is same across all interactions of dept and exp (we accept null)

# check if if trestbps changes with the change in cp and sex

trestbps <- aov(trestbps ~ cp + sex + cp*sex, data = heart)
summary(trestbps)

# check age differs significantly with change in cp and target

heart$cp <- as.factor(heart$cp)
heart$target <- as.factor(heart$target)
age <- aov(age ~ cp + target, data = heart)
summary(age)
