###classifying data with a support vector machine

install.packages("e1071")
library(e1071)
library(C50)
data(churn)

model=svm(churn~.,data=trainset,kernel="radial",cost=1,gamma=1/ncol(trainset))
summary(model)

##choosing the cost of a support vector machine

#subset the iris dataset
iris.subset=subset(iris,select=c("Sepal.Length","Sepal.Width","Species"),Species %in% c("setosa","virginica"))
#generate a scatter plot
plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width,col=iris.subset$Species,pch=19)
#train SVM with the cost = 1
svm.model=svm(Species ~ .,data=iris.subset,kernel='linear',cost=1,scale=FALSE)
#circle the support vector with blue circles
points(iris.subset[svm.model$index,c(1,2)],col="blue",cex=2)
#add a separation line on the plot
w=t(svm.model$coefs) %*% svm.model$SV
b=-svm.model$rho
abline(a=-b/w[1,2],b=-w[1,1]/w[1,2],col="red",lty=5)

#create another SVM where cost=10,000
plot(x=iris.subset$Sepal.Length,y=iris.subset$Sepal.Width,col=iris.subset$Species,pch=19)
svm.model=svm(Species ~ .,data=iris.subset,type='C-classification',kernel='linear',cost=10000,scale=FALSE)
points(iris.subset[svm.model$index,c(1,2)],col="blue",cex=2)
w=t(svm.model$coefs) %*% svm.model$SV
b=-svm.model$rho
abline(a=-b/w[1,2],b=-w[1,1]/w[1,2],col="red",lty=5)

##Visualizing an SVM fit

data(iris)
model.iris=svm(Species~.,iris)
plot(model.iris,iris,Petal.Width ~ Petal.Length,slice=list(Sepal.Width=3,Sepal.Length=4))

plot(model,trainset,total_day_minutes ~ total_intl_charge)

##predicting labels 
svm.pred=predict(model,testset[,!names(testset) %in% c("churn")])
svm.table=table(svm.pred,testset$churn)
svm.table

library(caret)
confusionMatrix(svm.table)


##train a support vector machine based a Quartet dataset

library(car)
data(Quartet)
model.regression=svm(Quartet$y1~Quartet$x,type="eps-regression")
predict.y=predict(model.regression,Quartet$x)
predict.y
plot(Quartet$x,Quartet$y1,pch=19)
points(Quartet$x,predict.y,pch=15,col="red")

##tuning a svm
tuned=tune.svm(churn~.,data=trainset,gamma=10^(-6:-1),cost=10^(1:2))
summary(tuned)
#retrain the SVM with the best parameter
model.tuned=svm(churn~.,data=trainset,gamma=tuned$best.parameters$gamma,cost=tuned$best.parameters$cost)
summary(model.tuned)

svm.tuned.pred=predict(model.tuned,testset[,!names(testset) %in% c("churn")])
svm.tuned.table=table(svm.tuned.pred,testset$churn)
svm.tuned.table

classAgreement(svm.tuned.table)
confusionMatrix(svm.tuned.table)
