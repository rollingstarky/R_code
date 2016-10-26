###Generalized linear model(GLM)


##applying the Gaussian model

library(car)
data(SLID)

lmfit1=glm(wages~age+sex+education,data = SLID,family=gaussian())
summary(lmfit1)

lmfit2=lm(wages~age+sex+education,data=SLID)
summary(lmfit2)

#compare the two fitted models
library(lmtest)
anova(lmfit1,lmfit2)


##applying the Poisson model

data(warpbreaks)
head(warpbreaks)

rsl=glm(breaks~tension,data=warpbreaks,family="poisson")
summary(rsl)


##applying the Binomial model

data(mtcars)
head(mtcars)

lm1=glm(vs~hp+mpg+gear,data=mtcars,family=binomial)
summary(lm1)


##fitting a generalized additive model

install.packages("mgcv")
library(mgcv)
install.packages("MASS")
library(MASS)
attach(Boston)
str(Boston)

fit=gam(dis~s(nox))
summary(fit)


##visualizing a generalized additive model

#generate a scatter plot
plot(nox,dis)
#add the regression
x=seq(0,1,length=500)
y=predict(fit,data.frame(nox=x))
lines(x,y,col="red",lwd=2)
#plot the fit model
plot(fit)

#a contour plot
fit2=gam(medv~crim+zn+crim:zn,data=Boston)
vis.gam(fit2)


##Diagnosing a generalized additive model

par(mfrow=c(2,2))
gam.check(fit)
