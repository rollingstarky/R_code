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
