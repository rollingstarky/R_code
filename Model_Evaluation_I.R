###Model Evaluation


##Estimating model performance with k-fold cross-validation

#split the index into 10 fold
library(e1071)
ind=cut(1:nrow(churnTrain),breaks=10,labels=F)
#use for loop to perform a 10 fold cross-validation 10 times
accuracies=c()
for(i in 1:10){
  fit=svm(churn ~.,churnTrain[ind !=i,])
  predictions=predict(fit,churnTrain[ind==i,!names(churnTrain) %in% c("churn")])
  correct_count=sum(predictions==churnTrain[ind==i,c("churn")])
  accuracies=append(correct_count/nrow(churnTrain[ind==i,]),accuracies)
}

#print the accuracies
accuracies
mean(accuracies)

##performing cross-validation with the e1071 package

tuned=tune.svm(churn~.,data=trainset,gamma=10^-2,cost=10^2,tunecontrol=tune.control(cross=10))
tuned$performances
#use the optimum model
svmfit=tuned$best.model
table(trainset[,c("churn")],predict(svmfit))

##performing cross-validation with the caret package

library(caret)
control=trainControl(method="repeatedcv",number=10,repeats = 3)
library(rpart)
model=train(churn~.,data=trainset,method="rpart",preProcess="scale",trControl=control)
model

##ranking the variable importance with the caret package

importance=varImp(model,scale=FALSE)
importance
plot(importance)

##ranking the variable importance with the rminer package
install.packages("rminer")
library(rminer)
model=fit(churn~.,trainset,model="svm")
VariableImportance=Importance(model,trainset,method="sensv")
L=list(runs=1,sen=t(VariableImportance$imp),sresponse=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(trainset),col="gray",Grid=10)

##finding highly correlated features with the caret package

#remove the features are not coded in numeric characters
new_train=trainset[,! names(churnTrain) %in% c("churn","international_plan","voice_mail_plan")]
#obtain the correlation of each attribute
cor_mat=cor(new_train)
highlyCorrelated=findCorrelation(cor_mat,cutoff = 0.75)
#obtain the name of highly correlated attributes
names(new_train)[highlyCorrelated]
