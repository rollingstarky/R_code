###Regression Analysis


##linear regression with lm

#install the car package
install.packages("car")
library(car)
#load Quartet dataset
data(Quartet)
str(Quartet)
#draw a scatter plot and append a fitted line
plot(Quartet$x,Quartet$y1)
lmfit=lm(y1~x,Quartet)
abline(lmfit,col="red")
#view the fit model
lmfit
#summary of the fitted model
summary(lmfit)


##predict unknown values

newdata=data.frame(x=c(3,6,15))
#using the confidence interval
predict(lmfit,newdata,interval = "confidence",level=0.95)
#using this prediction interval
predict(lmfit,newdata,interval = "predict")


##generating a diagnostic plot of a fitted model

par(mfrow=c(2,2))
plot(lmfit)


##fitting a polynomial regression model with lm

par(mfrow=c(1,1))
plot(Quartet$x,Quartet$y2)
lmfit=lm(Quartet$y2~poly(Quartet$x,2))
lines(sort(Quartet$x),lmfit$fit[order(Quartet$x)],col="red")


##fiting a robust linear regression model with rlm

plot(Quartet$x,Quartet$y3)
library(MASS)
lmfit=rlm(Quartet$y3~Quartet$x)
abline(lmfit,col="red")


##study a case of linear regression on SLID

data(SLID)
str(SLID)

#visualize the variable wages against ...
par(mfrow=c(2,2))
plot(SLID$wages~SLID$language)
plot(SLID$wages~SLID$age)
plot(SLID$wages~SLID$education)
plot(SLID$wages~SLID$sex)
#fit the model
lmfit=lm(wages~.,data=SLID)
summary(lmfit)
#drop the language attribute
lmfit=lm(wages~age+sex+education,data=SLID)
summary(lmfit)
#draw a diagnostic plot
plot(lmfit)
#take the log of wages and replot
lmfit=lm(log(wages) ~ age + sex + education,data=SLID)
plot(lmfit)
#diagnose the heteroscedasticity of the regression model
install.packages("lmtest")
library(lmtest)
bptest(lmfit)
#correct standard errors
install.packages("rms")
library(rms)
olsfit=ols(log(wages) ~ age+sex+education,data=SLID,x=TRUE,y=TRUE)
robcov(olsfit)
