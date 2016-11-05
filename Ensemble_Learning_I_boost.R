###Ensemble Learning

##Classifying data with the bagging method
install.packages("adabag")
library(adabag)

set.seed(2)
churn.bagging=bagging(churn ~ .,data=trainset,mfinal=10)
#access the variable importance
churn.bagging$importance
#use the predicted results
churn.predbagging=predict.bagging(churn.bagging,newdata = testset)
churn.predbagging$confusion

##performing cross-validation with the bagging method

churn.baggingcv=bagging.cv(churn~.,v=10,data=trainset,mfinal=10)
#retrieve the minimum estimation errors
churn.baggingcv$error

##classifying data with the boosting method

set.seed(2)
churn.boost=boosting(churn~.,data=trainset,mfinal = 10,coeflearn = "Freund",boos=FALSE,control=rpart.control(maxdepth = 3))
churn.boost.pred=predict.boosting(churn.boost,newdata = testset)
churn.boost.pred$confusion
churn.boost.pred$error

#use the caret package to perform a classification with the boosting method
library(mboost)
library(pROC)

set.seed(2)
ctrl=trainControl(method="repeatedcv",repeats=1,classProbs = TRUE,summaryFunction = twoClassSummary)
ada.train=train(churn ~ .,data=trainset,method="ada",metric="ROC",trControl=ctrl)
ada.train$result
plot(ada.train)

ada.predict=predict(ada.train,testset,"prob")
ada.predict.result=ifelse(ada.predict[1]>0.5,"yes","no")
table(testset$churn,ada.predict.result)

##performing cross-validation with the boosting method
churn.boostcv=boosting.cv(churn~.,v=10,data=trainset,mfinal=5,control=rpart.control(cp=0.01))
churn.boostcv$error

##classifying data with gradient boosting
install.packages("gbm")
library(gbm)

#transform yes/no response to numeric response(0/1)
trainset$churn=ifelse(trainset$churn=="yes",1,0)
set.seed(2)
churn.gbm=gbm(formula=churn~.,distribution = "bernoulli",data=trainset,n.trees=1000,interaction.depth=7,shrinkage=0.01,cv.folds=3)
summary(churn.gbm)
#obtain the best iteration
churn.iter=gbm.perf(churn.gbm,method="cv")
#retrieve the odd value of the log returned from the Bernouli loss function
churn.predict=predict(churn.gbm,testset,n.tress=churn.iter)
str(churn.predict)
#plot the ROC curve
churn.roc=roc(testset$churn,churn.predict)
plot(churn.roc)
#retrieve the best cut off with the coords function
coords(churn.roc,"best")
churn.predict.class=ifelse(churn.predict>coords(churn.roc,"best")["threshold"],"yes","no")
table(testset$churn,churn.predict.class)


##Calculating the margins of a classifier
#calculate the margins of the boosting classifiers
boost.margins=margins(churn.boost,trainset)
boost.pred.margins=margins(churn.boost.pred,testset)
#plot a marginal cumulative distribution graph of the boosting classifiers
plot(sort(boost.margins[[1]]),(1:length(boost.margins[[1]]))/length(boost.margins[[1]]),type="l",xlim=c(-1,1),main="Boosting:observations",col="blue")
lines(sort(boost.pred.margins[[1]]),(1:length(boost.pred.margins[[1]]))/length(boost.pred.margins[[1]]),type="l",col="green")
abline(v=0,col="red",lty=2)
#calculate the percentage of negative margin matches training and test errors
boosting.training.margin=table(boost.margins[[1]]>0)
boosting.negative.training=as.numeric(boosting.training.margin[1]/boosting.training.margin[2])
boosting.negative.training

boosting.testing.margin=table(boost.pred.margins[[1]]>0)
boosting.negative.testing=as.numeric(boosting.testing.margin[1]/boosting.testing.margin[2])
boosting.negative.testing

bagging.margins=margins(churn.bagging,trainset)
bagging.pred.margins=margins(churn.predbagging,testset)

#plot a margin cumulative distribution graph of the bagging classifiers
plot(sort(bagging.margins[[1]]),(1:length(bagging.margins[[1]]))/length(bagging.margins[[1]]),type="l",xlim=c(-1,1),main="Bagging:Margin cumulative distribution graph",xlab="margin",ylab="% observations",col="blue")
lines(sort(bagging.pred.margins[[1]]),(1:length(bagging.pred.margins[[1]]))/length(bagging.pred.margins[[1]]),type="l",col="green")
abline(v=0,col="red",lty=2)

bagging.training.margin=table(bagging.margins[[1]]>0)
bagging.negative.training=as.numeric(bagging.training.margin[1]/bagging.training.margin[2])
bagging.negative.training

bagging.testing.margin=table(bagging.pred.margins[[1]]>0)
bagging.negative.testing=as.numeric(bagging.testing.margin[1]/bagging.testing.margin[2])
bagging.negative.testing

##calculating the error evolution

boosting.evol.train=errorevol(churn.boost,trainset)
boosting.evol.test=errorevol(churn.boost,testset)
plot(boosting.evol.test$error,type="l",ylim=c(0,1),main="Boosting error versus number of trees",xlab="Iterations",ylab="Error",col="red",lwd=2)
lines(boosting.evol.train$error,cex=.5,col="blue",lty=2,lwd=2)
legend("topright",c("test","train"),col=c("red","blue"),lty=1:2,lwd=2)

##Classifying data with random forest
install.packages("randomForest")
library(randomForest)

churn.rf=randomForest(churn ~ .,data=trainset,importance=T)
churn.rf
churn.prediction=predict(churn.rf,testset)
table(churn.prediction,testset$churn)
plot(churn.rf)
importance(churn.rf)
varImpPlot(churn.rf)
margins.rf=margin(churn.rf,trainset)
plot(margins.rf)
hist(margins.rf,main="Margins of Random Forest for churn dataset")
boxplot(margins.rf~trainset$churn,main="Margins of Random Forest for churn dataset by class")

