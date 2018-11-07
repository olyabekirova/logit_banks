#install.packages('caret')

#install.packages("readr")

library(readr)

library(caret)

set.seed(1234)



# Setting file

# Reading in dataset

creditData <- read_csv("creditData.csv")

View(creditData)



#Running Summary

summary(creditData)



df <- creditData



#We want to check the types for each and decide what are the logical types



#yes or no

df$default <- as.factor(df$default)

#has bins

df$account_check_status <- as.factor(df$account_check_status)

#no of months

df$duration_in_month <- as.numeric(df$duration_in_month)

#credit status/history

df$credit_history <- as.factor(df$credit_history)

#purpose - 10 + categories

df$purpose <- as.factor(df$purpose)

# $$$

df$credit_amount <- as.numeric(df$credit_amount)

# bins/ranges of value savings

df$savings <- as.factor(df$savings)

# how long one has been with present employeer

df$present_emp_since <- as.factor(df$present_emp_since)

# How much each payment is going to eat up income in percent

df$installment_as_income_perc <- as.factor(df$installment_as_income_perc)

#sex & marital status

df$personal_status_sex <- as.factor(df$personal_status_sex)

#are you by yourself or a co-applicat or guarantor?

df$other_debtors <- as.factor(df$other_debtors)

#how much time you've lived in current house - Unknown range

df$present_res_since <- as.factor(df$present_res_since)

# type of property - i.e. real estate

df$property <- as.factor(df$property)

#age

df$age <- as.numeric(df$age)

#other installment plans

df$other_installment_plans <- as.factor(df$other_installment_plans)

#type of home/housing - i.e. rent, own

df$housing <- as.factor(df$housing)

#Unknown

df$credits_this_bank <- as.factor(df$credits_this_bank)

#job type

df$job <- as.factor(df$job)

#number of dependents

df$people_under_maintenance <- as.numeric(df$people_under_maintenance)

#telephone?

df$telephone <- as.factor(df$telephone)

#foreign worker?

df$foreign_worker <- as.factor(df$foreign_worker)





#Viewing age distribution

counts <- table(df$age)

barplot(counts, main="Age Distribution",

        xlab="Age")



#Purpose

counts <- table(df$purpose)

barplot(counts, main="What was the purpose of the loan?",

        xlab="Purpose")





#Approved and not approved - Changing labels

#df$default <- factor(df$default,

#                    levels = c(0,1),

#                   labels = c("Approved", "Not Approved"))



#Default?

counts <- table(df$default)

barplot(counts, main="Result of loan",

        xlab="default")



table(df$default)



# Now we are going to split our training and testing sets by 70/30 



trainIndex <- createDataPartition(df$default, p=.70,

                                  list=FALSE,

                                  times=1)

training = df[trainIndex,]

test = df[-trainIndex,]



#install.packages("Amelia")

library(Amelia)

missmap(training,main = "Missing values vs. Observed")



logisticModel <- glm(default ~ account_check_status +

                               duration_in_month +

                               credit_history +

                               purpose + 

                               credit_amount +

                               savings +

                               present_emp_since +

                               installment_as_income_perc +

                               personal_status_sex +

                               other_debtors +

                               present_res_since +

                               property + 

                               age +

                               other_installment_plans +

                               housing +

                               credits_this_bank +

                               job +

                               people_under_maintenance +

                               telephone +

                               foreign_worker,

                  family = binomial(link='logit'),

                  data = training

                               )

                                  

#install.packages("pscl")

library(pscl)



#install.packages("car")

library(car)



#Shows coefficents and p-values



summary(logisticModel)



#variable importance

varImp(logisticModel)



#VIF

vif(logisticModel)





#---------------------Second round of Logistic Regression after 

# reviewing stats and removing variables that are not

# significant or cause multicolinearity.





# ------ Non signficant variables

# - purpose, savings, property, housing, job, telephone



logisticModel <- glm(default ~ account_check_status +

                       duration_in_month +

                       credit_history +

                       #purpose + 

                       credit_amount +

                       #savings +

                       present_emp_since +

                       installment_as_income_perc +

                       personal_status_sex +

                       other_debtors +

                       present_res_since +

                       #property + 

                       age +

                       other_installment_plans +

                       #housing +

                       credits_this_bank +

                       #job +

                       people_under_maintenance +

                       #telephone +

                       foreign_worker,

                     family = binomial(link='logit'),

                     data = training

)





#install.packages("pscl")

library(pscl)



#install.packages("car")

library(car)



#Shows coefficents and p-values



summary(logisticModel)



#variable importance

varImp(logisticModel)



#VIF

vif(logisticModel)





#---------------------Third round of Logistic Regression after 

# reviewing stats and removing variables that are not

# significant or cause multicolinearity.





# ------ Non signficant variables

# - present_emp_since,installment_as_income_perc,credits_this_bank,age,other_debtors



logisticModel <- glm(default ~ account_check_status +

                       duration_in_month +

                       credit_history +

                       #purpose + 

                       credit_amount +

                       #savings +

                       #present_emp_since +

                       #installment_as_income_perc +

                       personal_status_sex +

                       #other_debtors +

                       present_res_since +

                       #property + 

                       #age +

                       other_installment_plans +

                       #housing +

                       #credits_this_bank +

                       #job +

                       people_under_maintenance +

                       #telephone +

                       foreign_worker,

                     family = binomial(link='logit'),

                     data = training

)





#install.packages("pscl")

library(pscl)



#install.packages("car")

library(car)



#Shows coefficents and p-values



summary(logisticModel)



#variable importance

varImp(logisticModel)



#VIF

vif(logisticModel)



#------------------ THIS IS THE FINAL VERSION OF OUR LOGISTIC REGRESSION AND THE FOLLOWING RESULTS INCLUDING 

#CONFUSTION MATRIX, ROC CURVE, AND OTHER MEASURES.







#---------------------Fourth round of Logistic Regression after 

# reviewing stats and removing variables that are not

# significant or cause multicolinearity.





# ------ Non signficant variables

# - present_emp_since,installment_as_income_perc,credits_this_bank,age,other_debtors



logisticModel <- glm(default ~ account_check_status +

                       duration_in_month +

                       credit_history +

                       #purpose + 

                       #credit_amount +

                       #savings +

                       #present_emp_since +

                       #installment_as_income_perc +

                       personal_status_sex +

                       #other_debtors +

                       present_res_since +

                       #property + 

                       #age +

                       other_installment_plans +

                       #housing +

                       #credits_this_bank +

                       #job +

                       people_under_maintenance +

                       #telephone +

                       foreign_worker,

                     family = binomial(link='logit'),

                     data = training

)





#install.packages("pscl")

library(pscl)



#install.packages("car")

library(car)



#Shows coefficents and p-values



summary(logisticModel)



logit(logisticModel)



#variable importance

varImp(logisticModel)



#VIF

vif(logisticModel)





#-------- Checking predicted scores





#Predicted scores

fitLog <- predict(logisticModel,type="response",newdata=test, na.action = na.pass)

length(fitLog)

fitLog



#Using optimal cutoff function to find the optimal threshold

#for prediction probability score.



#install.packages("InformationValue")

library(InformationValue)

#install.packages("gplots")

library(gplots)



opt1 <- optimalCutoff(test$default,fitLog)

opt1

test$default



counts <- table(training$default)

barplot(counts, main="DEFAULT DISTRIBUTION",

        xlab="Approved Vs. Not Approved") 

                                





#Calculate Accuracy, using the optimal cutoff

fitLog<- ifelse(fitLog > opt1,1,0)

misclassificationError1 <- mean(fitLog != test$default)

fitLog

print(paste('Model Accuracy',1-misclassificationError1))



#Reset Fitlog

fitLog <- predict(logisticModel,type="response",newdata=test)





#install.packages("ROCR")

library(ROCR)

#AUC Curve

predLog = prediction(fitLog, test$default)





#Confustion matrix

confusionMatrix(test$default, fitLog)

#caret::confusionMatrix(test$default, reference = fitLog)





length(test$default)

length(fitLog)





perfLog <- performance(predLog, "tpr", "fpr")

plot(perfLog, col="red", lwd=1)

AUCLog1=performance(predLog, measure = "auc")@y.values[[1]]

cat("AUC: ",AUCLog1,"n")
