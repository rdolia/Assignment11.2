#Importing marketing dataset.
bank_data<- read.csv("bank-full.csv",1,sep = ';')
bank_data<-as.data.frame(bank_data)

#Perform the below operations:
# a. Create a visual for representing missing values in the dataset.
options(max.print=999999)
library(naniar)
na_strings <- ("unknown")
na_strings <-as.character(na_strings)
#creating new set with missing value unknown changed to NA.
bank_data1<- bank_data %>% replace_with_na_all(condition = ~.x %in% na_strings)

#visualising missing values
vis_miss(bank_data1)

#remove/impute missing values
#As per the above visualisation poutcome variable has 81 % of data missing
#hence removing that variable.
bank_data2 <-bank_data1[-16]

#replace NA in contact column by 0

bank_data2[["contact"]][is.na(bank_data2[["contact"]])] <- 0

#job and eduducation are the only columns now with missing data.
#imputing data with mice package.
library(mice)
md.pattern(bank_data2)

head(bank_data2)
library(dplyr) 
bank_data2 <- bank_data2 %>%
  mutate(
    job = as.factor(job),
    education = as.factor(education),
    marital = as.factor(marital),
    default = as.factor(default),
    housing = as.factor(housing),
    loan = as.factor(loan),
    contact = as.factor(contact)
  )
str(bank_data2)
#running the mice function
bank_data3 <- mice(bank_data2,m=3,maxit=10,seed=500)
summary(bank_data3)
#check for imputed data in a field job
bank_data3$imp$job
#check for imputed data in a field education
bank_data3$imp$education
#Now we can get back the completed dataset using the complete() function.
bank_data4 <- complete(bank_data3,1)
md.pattern(bank_data4)


#a. Is there any association between job and default?
#$converting job, default to numeric
bank_data4 <- bank_data4 %>%
  mutate(
    job = as.numeric(job),
    default = as.numeric(default)
      )

#Calculating Spearman rho correlation
corra<- cor.test(bank_data4$job,bank_data4$default,method = 'spearman')
corra
#rho calculated between job and default is -0.004 , hence we can conclude
#that there is very low negative correlation between the two variables.
#b. Is there any significant difference in duration of last call between?
#  people having housing loan or not?

#finding average of duration for loan value 1 and 2.
library(sqldf)
loan1<- sqldf("SELECT duration 
      FROM  bank_data4
              WHERE `loan` = 1")
#calculate mu1 which is the mean of duration with loan = 1
x1bar<- mean(loan1$duration)
x1bar
 
#calculate mu2 which is the mean of duration with loan = 2
loan2<- sqldf("SELECT duration 
      FROM  bank_data4
              WHERE `loan` = 2")
x2bar<-mean(loan2$duration)
x2bar
#calculation standard deviation for both samples loan1 and loan2
temp1 <- (loan1$duration)
sd1<- sd(temp1)
sd1
temp2 <-loan2$duration
sd2<-sd(temp2)
sd2
#calculate t

numerator = x1bar - x2bar

#denominator

denominator <- sqrt((sd1^2)/37967 + (sd2^2)/7244)
#hence t is 
t<-numerator/denominator
t
#t value is 2.67.
#Considering significance of 0.05 we have critical z value as -+1.96.
#As calculated z value 2.67 is bigger than 1.96 we reject the null hypothesis 
#and conclude that there is a significant difference in duration of last call
#between people having housing loan and not.

#d. Is the employment variation rate consistent across Job types?
bank_data4 <- bank_data4 %>%
  mutate(
    job = as.factor(job)
      )

levels(bank_data4$job)
 #there are 11 job types. 
EVRnum <- table(bank_data4$job)
EVRnum
#total number of people is 45211 hence EVR is 
EVR<-EVRnum/45211 *100
EVR
#From the above Employment variation rate table  calculated we can see that 
# the variation rate varies for each job hence we can conclude 
#that they are not consistent across Job types.

#e. Is the employment variation rate same across Education?
#creating 3 sets with educations 1 2 and 3.
education1<- sqldf("SELECT job 
      FROM  bank_data4
      WHERE `education` = 1")
education1
education2<- sqldf("SELECT job 
      FROM  bank_data4
                   WHERE `education` = 2")

education3<- sqldf("SELECT job 
      FROM  bank_data4
                   WHERE `education` = 3")
#displaying proportions of job type in each table and calculating 
#EVR for each table of education
EVRnum1 <- table(education1)
EVRnum1<-EVRnum1/45211*100


EVRnum2 <-table(education2)
EVRnum2<-EVRnum2/45211*100


EVRnum3 <- table(education3)
EVRnum3 <-EVRnum3/45211*100


EVRgroup <- cbind(EVRnum1,EVRnum2,EVRnum3)
EVRgroup
#Hence we can see that the employment variation rate is not same across Education
