##measuring the performance of the regression model
library(caret)
library(car)
data(Quartet)
#plot the attribute,y3,against x using the lm function
plot(Quartet$x,Quartet$y3)
lmfit=lm(Quartet$y3~Quartet$x)
abline(lmfit,col="red")
#retrieve predicted value
predicted=predict(lmfit,newdata = Quartet[c("x")])
#calculate the root mean square error
actual=Quartet$y3
rmse=(mean((predicted-actual)^2))^0.5
rmse
#calculate the relative square error
mu=mean(actual)
rse=mean((predicted-actual)^2)/mean((mu-actual)^2)
rse
#use R-Square as a measurement
rsquare=1-rse
rsquare
#plot attribute,y3,against x using the rlm function
library(MASS)
plot(Quartet$x,Quartet$y3)
rlmfit=rlm(Quartet$y3~Quartet$x)
abline(rlmfit,col="red")

predicted=predict(rlmfit,newdata=Quartet[c("x")])
actual=Quartet$y3
rmse=(mean((predicted-actual)^2))^0.5
rmse
mu=mean(actual)
rse=mean((predicted-actual)^2)/mean((mu-actual)^2)
rse
rsquare=1-rse
rsquare

##Measuring prediction performance with a confusion matrix

#train a svm model
svm.model=train(churn ~ .,data=trainset,method="svmRadial")
svm.pred=predict(svm.model,testset[,!names(testset) %in% c("churn")])
table(svm.pred,testset[,c("churn")])
confusionMatrix(svm.pred,testset[,c("churn")])

##Measuring prediction performance using ROCR
install.packages("ROCR")
library(ROCR)
library(e1071)

svmfit=svm(churn ~ .,data=trainset,prob=TRUE)
pred=predict(svmfit,testset[,!names(testset) %in% c("churn")],probability=TRUE)
#obtain the probability of labels with yes
pred.prob=attr(pred,"probabilities")
pred.to.roc=pred.prob[,2]
#use the performance function to obtain the performance measurement
pred.rocr=prediction(pred.to.roc,testset$churn)
perf.rocr=performance(pred.rocr,measure="auc",x.measure = "cutoff")
perf.tpr.rocr=performance(pred.rocr,"tpr","fpr")
#Visualize the ROC curve using the plot function
plot(perf.tpr.rocr,colorize=T,main=paste("AUC:",(perf.rocr@y.values)))

##comparing an ROC curve using the caret package
install.packages("pROC")
library("pROC")

#set up the training control with a 10-fold cross-validation in 3 repetitions
control=trainControl(method="repeatedcv",number = 10,repeats=3,classProbs=TRUE,summaryFunction = twoClassSummary)
#train classifier
glm.model=train(churn ~.,data=trainset,method="glm",metric="ROC",trControl=control)
svm.model=train(churn ~.,data=trainset,method="svmRadial",metric="ROC",trControl=control)
rpart.model=train(churn ~.,data=trainset,method="rpart",metric="ROC",trControl=control)
#make predictions based on different trained models
glm.probs=predict(glm.model,testset[,!names(testset) %in% c("churn")],type="prob")
svm.probs=predict(svm.model,testset[,!names(testset) %in% c("churn")],type="prob")
rpart.probs=predict(rpart.model,testset[,!names(testset) %in% c("churn")],type="prob")
#generate the ROC curve of each model
glm.ROC=roc(response=testset[,c("churn")],predictor=glm.probs$yes,levels=levels(testset[,c("churn")]))
plot(glm.ROC,type="S",col="red")

svm.ROC=roc(response=testset[,c("churn")],predictor=svm.probs$yes,levels=levels(testset[,c("churn")]))
plot(svm.ROC,add=TRUE,col="green")

rpart.ROC=roc(response=testset[,c("churn")],predictor = rpart.probs$yes,levels=levels(testset[,c("churn")]))
plot(rpart.ROC,add=TRUE,col="blue")

##Measuring performance differences between models

#resample the three generated models
cv.values=resamples(list(glm=glm.model,svm.model,rpart=rpart.model))
summary(cv.values)
#use dotplot to plot the resampling result in the ROC metric
dotplot(cv.values,metrix="ROC")
bwplot(cv.values,layout=c(3,1))
