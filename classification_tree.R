###Classification--Tree


##preparing the datasets

install.packages("C50")
library(C50)
data(churn)
str(churnTrain)

#remove the attributes are not appropriate for classification features
churnTrain=churnTrain[,! names(churnTrain) %in% c("state","area_code","account_length")]
#split the data into training and testing dataset
set.seed(2)
ind=sample(2,nrow(churnTrain),replace=TRUE,prob=c(0.7,0.3))
trainset=churnTrain[ind==1,]
testset=churnTrain[ind==2,]
#use dim to explore the dimensions of the datasets
dim(trainset)
dim(testset)

#combine the split process into the split.data function
split.data=function(data,p=0.7,s=666){
  set.seed(s)
  index=sample(1:dim(data)[1])
  train=data[index[1:floor(dim(data)[1] * p)],]
  test=data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]],]
  return(list(train=train,test=test))
}


##Building a classification model with recursive partitioning trees

install.packages("rpart")
library(rpart)
#build a classification tree model
churn.rp=rpart(churn ~ .,data=trainset)
#retrive the node detail of the classification tree
churn.rp
#use the printcp function to examine the complexity parameter
printcp(churn.rp)
#use the plotcp function to plot the cost the cost complexity parameters
plotcp(churn.rp)
#examine the built model
summary(churn.rp)


##Visualizing a recursive partitioning tree

plot(churn.rp,margin = 0.1)
text(churn.rp,all=TRUE,use.n=TRUE)

#adjust the layout
plot(churn.rp,uniform = TRUE,branch = 0.6,margin=0.1)
text(churn.rp,all = TRUE,use.n = TRUE)


##Measuring the prediction performance

predictions=predict(churn.rp,testset,type="class")
table(testset$churn,predictions)
#generate a confusion matrix
install.packages("caret")
library(caret)
confusionMatrix(predictions,testset$churn)


##Pruning a recursive partitioning tree

#find the minimum cross-validation error of the classification tree model
min(churn.rp$cptable[,"xerror"])
#locate the record with the minimum cross-validation error
which.min(churn.rp$cptable[,"xerror"])
#get the cost complexity parameter of the record with the minimum cross-validation errors
churn.cp=churn.rp$cptable[7,"CP"]
churn.cp
#Prune the tree by the cp parameter to the CP value
prune.tree=prune(churn.rp,cp=churn.cp)
plot(prune.tree,margin=0.1)
text(prune.tree,all=TRUE,use.n = TRUE)
#generate a classification table based on the pruned classification tree model
predictions=predict(prune.tree,testset,type="class")
table(testset$churn,predictions)
#generate a confusion matrix 
confusionMatrix(table(predictions,testset$churn))


##building a classification model with a conditional inference tree

#use the ctree to build the classification model
install.packages("party")
library(party)
ctree.model=ctree(churn ~ .,data=trainset)
ctree.model
#visualizing a conditional inference tree
plot(ctree.model)
#reduce the built model with less input features
daycharge.model=ctree(churn ~ total_day_charge,data=trainset)
plot(daycharge.model)

##measuring the prediction performance of a conditional inference tree

ctree.predict=predict(ctree.model,testset)
table(ctree.predict,testset$churn)
confusionMatrix(table(ctree.predict,testset$churn))
#use the treeresponse function,the list of class probabilities
tr=treeresponse(ctree.model,newdata=testset[1:5,])
tr
