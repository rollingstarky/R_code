###training a nerual network with neuralnet

data(iris)
ind=sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
trainset=iris[ind==1,]
testset=iris[ind==2,]

install.packages("neuralnet")
library(neuralnet)

#add the columns versicolor
trainset$setosa=trainset$Species=="setosa"
trainset$virginica=trainset$Species=="virginica"
trainset$versicolor=trainset$Species=="versicolor"
#train the neural network with three hidden neurons in each layer
network=neuralnet(versicolor + virginica + setosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,data=trainset,hidden=3)
network
head(network$generalized.weights[[1]])

##Visualizing a neural network
plot(network)
#use gwplot to visualize the generalized weights
par(mfrow=c(2,2))
gwplot(network,selected.covariate = "Petal.Width")
gwplot(network,selected.covariate = "Sepal.Width")
gwplot(network,selected.covariate = "Petal.Length")
gwplot(network,selected.covariate = "Petal.Width")

##Predicting labels
#generate a prediction probability matrix 
net.predict=compute(network,testset[-5])$net.result
#obtain other possible labels by finding the column with the greasted probability
net.prediction=c("versicolor","virginica","setosa")[apply(net.predict,1,which.max)]
#generate a classification table
predict.table=table(testset$Species,net.prediction)
predict.table
library(caret)
confusionMatrix(predict.table)


##train a neural network with nnet
install.packages("nnet")
library(nnet)
data(iris)
set.seed(2)

ind=sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
trainset=iris[ind==1,]
testset=iris[ind==2,]

iris.nn=nnet(Species ~ .,data=trainset,size=2,rang=0.1,decay=5e-4,maxit=200)
summary(iris.nn)

#predicting lables
iris.predict=predict(iris.nn,testset,type="class")
nn.table=table(testset$Species,iris.predict)
nn.table
confusionMatrix(nn.table)
