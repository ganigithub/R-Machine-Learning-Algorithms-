# R has 5 constants that can be viewed in console
pi 
month.abb
month.name
letters
LETTERS
 
#variable is a place holder  #values of variable can be viewed in global environment
a = 1

b <- 3+3  # '<-' can be used in place of equal for assignment.
b < -3+3  # there shouldn't be space bet '<' and '-' or the meaning will be changed

a = a*3   # this will not generate result but store.
a         # this is called 'calling variable'

rm(a)     # this will remove or delete variable 'a'

a2 = 4    # variable name should start from a letter

# int a = 34 # in some languages, we need to give type of data. this is called 'strongly typed language'
# Python and R don't need to be given type of data.

#R supports char, numeric, int, logical and complex types of data.

x = 'hi'
class(x)

x = 2    #numeric sotres float, fracitons
class(x)

x= 3.2
class(x)

x=TRUE
class(x)   # Logical

x = 3i+2   # Complex
class(x)

b = 100L   #L represents literal. R stores 100 as an integer and not numeric
class(b)

a = 3.3L
class(a)   # this shows a warning but is converted into numeric and stores

## data structures in R/ collective data types:

# vector, list, data frame, array, matrix


# vector: collection of one dimentional homogeneous data elements. (mutable)

vect1 <- c(10,20,30,40,50,60,70,80,90,100)   # c stands for concatenation / collection / combination
class(vect1)

#indexing starts from 1 in R
vect1[1]

vect1[1:3]     #6 is inclusive
vect1[c(1,3)]  # gives no at 1st and 3rd position
vect1[-3]      # it removes the 3rd element

vact2 <- sample(100, 15)  #gives 15 random values in range(0-100)

#get values present at odd and even index position:
vect1[seq(1,15,2)]  #odd
vect1[seq(2,15,2)]  #even

#get elements at index positions 8-10 and then 3-8:
vect1[c(8:10, 3:8)]


#add one element at end of vect1:
vect1[11]<-110
vect1

#add more than one element to vect1:
vect1[12:15] <- c(120, 130, 140, 150)
vect1

#add 300 at 25th index:
vect1[25] <- 300
vect1

#replace:
vect1[24] <- 290
vect1

class(vect1) #class remains numeric even though there are NA 

#replace more then one element:

vect1[c(10,20,25)] <- c(1,2,3); vect1

#removing values at certain index: delete value at index 2:

vect1[-2]
vect1 #notice 2nd element is still present in vect1

vect1 = vect1[-2]  #to remove permanently
vect1

#remove 5 to 11:
vect1 = vect1[-(5:11)]
vect1


#vectors can only store one type of data types (homogenous):

vect3 <- c(10, 20L, TRUE, 1+2i, 'hello')
vect3

class(vect3)          #class is character. R can and converts all elements to characters called implicit conversion

vect4 <- c(1, 1L, TRUE, 3+4i)
class(vect4)          #since each element can be converted to complex, class is complex.

vect5 <- c(12, 30L, TRUE)
class(vect5)          #each element is converted to numeric

vect6 <- c(2L, T)
class(vect6)          # element is converted to integer

# The heirarchy of data types : char - complex - num - int - logical

# complex, num, int, logical can be converted to 'character'
# num, int, logical can be converted to 'complex', etc


## Factor

gender <- c('M', 'F', 'F', 'M', 'M', 'M', 'F', 'F')
length(gender)
summary(gender)

gender_fact <- as.factor(gender)   #as.factor is used for grouping
gender_fact
summary(gender_fact)



## Stats: types of data : continuous and categorical

 # Types of continuous data : ratio and interval

 # Types of categorical data : ordinal and nominal

          # nominal: binary, multi-nominal





## Data Frame : collection of multiple vectors. e.g Excel table, SQL table
# data frame is bi-dimensional

#create data frame with labels: id, name, gender, loc, salary, bonus

id <- 1:10  #if first value is 1, R will consider it as numerical data
name <- c('a','b', 'c', 'd','e', 'f', 'g', 'h', 'i','j')
gender <- c('M','M','M','M','M','F','F','F','F','F')
loc <- c('Mum','Mum','Mum','Mum','Mum','Pune','Pune','Pune','Pune','Pune')
salary <- c(4000,4000,4000,4000,4000,5000,5000,5000,5000,5000)
bonus <- c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000)

emp <- data.frame(id,name, gender, loc, salary, bonus)
emp

summary(emp)  #gives summary of data frame
names(emp)    # gives column labels

emp1 <- data.frame(id, name, gender, loc, salary, bonus, stringsAsFactors = T)  #StringsAsFactors takes char's as factors
summary(emp1)

#if we don't want to convert name as factor:
emp1$name <- as.character(emp$name)
summary(emp1)

# subsetting: slicing
 # slicing on rows: fetch rows from 2 to 5

emp1[2:5, ] #before comma fetches rows, and after comma for columns

 #slicing on columns: fetch columns of name and id
emp1[, 1:2]

emp1[,c(1,6)]  #id and bonus
emp1[,c('bonus','id')]

 # fetch name and id for rows 3 to 6
emp1[3:6, c('name','id')]

fix(emp1) #we can edit data using this.



save.image(file = 'C:/Users/ganesh/Documents/R files/program1.RData')