###Basics

##manipulate data

#load the dataset iris
data(iris)
#index by name(rows,columns)
iris[1,"Sepal.Length"]
#select multiple columns using c()
Sepal.iris=iris[,c("Sepal.Length","Sepal.Width")]
#summarize the structure of Sepal
str(Sepal.iris)
#subset data
Five.Sepal.iris=iris[1:5,c("Sepal.Length","Sepal.Width")]
str(Five.Sepal.iris)
#filter the data
setosa.data=iris[iris$Species=="setosa",1:5]
str(setosa.data)
#subset data using the subset function
Sepal.data=subset(iris,select=c("Sepal.Length","Sepal.Width"))
str(Sepal.data)

setosa.data=subset(iris,Species=="setosa")
str(setosa.data)
#merging data
flower.type=data.frame(Species="setosa",Flower="iris")
merge(flower.type,iris[1:3,],by="Species")
#ordering data
head(iris[order(iris$Sepal.Length,decreasing = TRUE),])



##basic statistics


#the format of the data
class(iris)
#descriptive statistics
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
median(iris$Sepal.Length)
range(iris$Sepal.Length)
quantile(iris$Sepal.Length)
#sapply function
sapply(iris[1:4],mean,na.rm=TRUE)
#full range of descriptive statistics
summary(iris)
#investigate the relationship between variables
cor(iris[,1:4])
#covariance of each attribute pair
cov(iris[,1:4])



##visualizing data

#calculate the frequency of species
table.iris=table(iris$Species)
table.iris
#simple pie chart
pie(table.iris)
#histogram of the sepal length
hist(iris$Sepal.Length)
#Boxplot function
boxplot(Petal.Width ~ Species,data=iris)
#scatter plot
plot(x=iris$Petal.Length,y=iris$Petal.Width,col=iris$Species)
#scatter plot with function 'pairs'
pairs(iris[1:4],main="Iris Data",pch=21,bg=c("red","green","blue")[unclass(iris$Species)])



