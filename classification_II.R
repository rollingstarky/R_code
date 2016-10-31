### k-nearest nerghbor classifier


install.packages("class")
library(class)
library(C50)

#replace yes and no to 1 and 0
levels(trainset$international_plan)=list("0"="no","1"="yes")
levels(trainset$voice_mail_plan)=list("0"="no","1"="yes")
levels(testset$international_plan)=list("0"="no","1"="yes")
levels(testset$voice_mail_plan)=list("0"="no","1"="yes")

#use knn on the trainset and testset
churn.knn=knn(trainset[,! names(trainset) %in% c("churn")],testset[,! names(testset) %in% c("churn")],trainset$churn,k=3)
summary(churn.knn)

#generate the classification matrix 
table(testset$churn,churn.knn)
#generate a confusion matrix
library(caret)
confusionMatrix(table(testset$churn,churn.knn))


###classifying data with logistic regression

fit=glm(churn ~ .,data=trainset,family=binomial)
summary(fit)

##use significant variables only
fit=glm(churn ~ international_plan + voice_mail_plan + total_intl_calls + number_customer_service_calls,data=trainset,family=binomial)
summary(fit)
pred=predict(fit,testset,type="response")
Class=pred>.5
summary(Class)

tb=table(testset$churn,Class)
tb

#turn the statistics of the previous step into a classification table
churn.mod=ifelse(testset$churn=="yes",1,0)
pred_class=churn.mod
pred_class[pred<=.5]=1-pred_class[pred<=.5]
ctb=table(churn.mod,pred_class)
ctb

##Naive Bayes classifier

library(e1071)
classifier=naiveBayes(trainset[,!names(trainset) %in% c("churn")],trainset$churn)
classifier

bayes.table=table(predict(classifier,testset[,!names(testset) %in% c("churn")]),testset$churn)
bayes.table

confusionMatrix(bayes.table)
