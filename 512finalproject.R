library(haven)
library(tidyverse)
library(leaps)
library(car)
library(dplyr)
library(olsrr)
library(glmnet)
library(MASS)
library(robustbase)

#read in xpt files

bodyfat<-read_xpt(file = f, col_select = NULL, skip = 0, n_max = Inf, .name_repair = "unique")
sleep<-read_xpt(file = g, col_select = NULL, skip = 0, n_max = Inf,  .name_repair = "unique")
nutrition<-read_xpt(file = h, col_select = NULL, skip = 0, n_max = Inf,  .name_repair = "unique")
alcohol<-read_xpt(file = i, col_select = NULL, skip = 0, n_max = Inf,  .name_repair = "unique")
demo<-read_xpt(file = j, col_select = NULL, skip = 0, n_max = Inf,  .name_repair = "unique")
measures<-read_xpt(file = k, col_select = NULL, skip = 0, n_max = Inf,  .name_repair = "unique")


#taking only the variables we want and ommitting NA observations

demo1 <- na.omit(demo[, c("SEQN", "RIAGENDR", "RIDAGEYR")])
sleep1 <- na.omit(sleep[, c("SEQN", "SLQ030", "SLD012")])
nutrition1 <- na.omit(nutrition[, c("SEQN", "DBD905", "DBD900", "DBD910")])
alcohol1 <- na.omit(alcohol[, c("SEQN", "ALQ130")])
measures1 <- na.omit(measures[, c("SEQN", "BMXWT", "BMXBMI", "BMXWAIST")])

#recoding gender variable to 1 for female, 0 for male

demo1$sex<-ifelse(demo1$RIAGENDR=="1",0,1)
demo1 <- demo1[, c("SEQN", "RIDAGEYR", "sex")]

#recoding snore variable to 1 for frequently snoring, 0 for not
sleep1$snore<-ifelse(sleep1$SLQ030=="3", 1, 0)
sleep1 <- sleep1[, c("SEQN", "SLD012", "snore")]

#removing age 80 and up from demo
demo1 <- demo1[-(which(demo1$RIDAGEYR %in% "80")),]

#response variable

visceraltissue<-bodyfat[, c("SEQN", "DXXVFATM")]

#merging one column at a time because i kept getting an error when i tried to all at once

merge1<-merge(demo1, nutrition1)
merge2<-merge(merge1, sleep1)
merge3<-merge(merge2, alcohol1)
merge4<-merge(merge3, measures1) 
merge5<-merge(merge4, visceraltissue)

fulldata<- na.omit(merge5)

#renaming variables

names(fulldata) <- c("ID", "AGE", "FEMALE", "READY", "FASTFOOD", "FROZEN", "SLEEP", "SNORE", "ALCOHOL",
                     "WEIGHT", "BMI", "WAIST", "VAT")

#AGE: includes participants 0-79
#FEMALE: 1 for female, 0 for male
#READY: Number of meals not home prepared in last 30 days
#FASTFOOD: Number of fast food/pizza meals eaten in  last 30 days
#FROZEN:  Number of frozen meals eaten in last 30 days
#SNORE: 1 for snores frequently, 0 for does not
#SLEEP: Average number of hours slept on workdays
#ALCOHOL: Average number of drinks per day in the past year
#WEIGHT:  Weight (kg)
#BMI: BMI (kg/m^2)
#WAIST: Waist circumfrence (cm)
#VAT MASS: Visceral adipose tissue mass

#removing id number
finalset<- fulldata[, c("AGE", "FEMALE",  "READY", "FASTFOOD", "FROZEN", "SLEEP", "SNORE", "ALCOHOL",
                                "WEIGHT", "BMI", "WAIST", "VAT")]


#leaving out last 200 observations for testing

trainingdata<-finalset[1:1511,]
testingdata<- finalset[1512:1711,]
  
#correlation matrix and scatterplot matrix
#Any nonlinear relationships? Collinearity? 

cor(trainingdata)
pairs(trainingdata)

#variable selection
#best subsets procedure

regfit.subset <- regsubsets(VAT~., data=trainingdata, nvmax=12)
reg.summary<- summary(regfit.subset)

#plotting for each criteria to see how many variables should be included

plot(reg.summary$rss,xlab="Number of Variables",ylab="SSE", type="l")

plot(reg.summary$adjr2,xlab="Number of Variables", ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2
       [which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
points(which.min(reg.summary$cp),reg.summary$cp
       [which.min(reg.summary$cp)],col="red",cex=2,pch=20)

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(which.min(reg.summary$bic),reg.summary$bic
       [which.min(reg.summary$bic)],col="red",cex=2,pch=20)

#adjusted rsq: 7
#mallow's cp: 7
#bic: 6

#6 variable model: age, female,  sleep,  snore, weight, waist
#7 variable model: age, female, sleep, snore, weight, BMI, waist


#trying forward selection and backward elimination

fitall<-lm(VAT ~ ., data=trainingdata)
forwardstart<-lm(VAT ~ 1, data=trainingdata)

forwardstep<-step(forwardstart, direction="forward", scope=formula(fitall))
backelim<-step(fitall, direction="backward")

#both chose same 7 variable model: waist, age, snore, weight, female, sleep, BMI


#possible fits

fit1 <- lm(VAT ~ AGE+FEMALE+SLEEP+SNORE+WEIGHT+WAIST, data=trainingdata)
fit2 <- lm(VAT ~ AGE+FEMALE+SLEEP+SNORE+WEIGHT+BMI+WAIST, data=trainingdata)

#third fit for fun (not chosen by any selection procedures)

fit3 <- lm(VAT ~ AGE+FEMALE+FASTFOOD+SLEEP+SNORE+WEIGHT+BMI+WAIST, data=trainingdata)


#comparing normality of errors for all 3
#all 3 models have problems with normality
hist(fit1$residuals)
hist(fit2$residuals)
hist(fit3$residuals)

qqPlot(fit1)
qqPlot(fit2)
qqPlot(fit3)


#continuing with fit 2, assessing constancy of error variance
errors<-fit2$residuals
fittedvalues<-fit2$fitted.values
plot(fittedvalues, errors) #megaphone shape, bad

abserrors<-abs(fit2$residuals)
plot(fittedvalues, abserrors)

#errors have a small problem with nonindependence (mildly correlated with fitted values)
#departure from normality + nonconstant error variance will likely need remedial action
error.independence <- lm(abserrors~fittedvalues)

#box-cox transformation

b <- boxcox(fit2)

# Extract lambda

lambda <- b$x[which.max(b$y)]

#applying transformation on y

trainingdata$vat.lambda = (trainingdata$VAT)^lambda
testingdata$vat.lambda = (testingdata$VAT)^lambda
boxcox.fit <- lm(vat.lambda ~ AGE+FEMALE+SLEEP+SNORE+WEIGHT+BMI+WAIST, data=trainingdata)

#checking normality and constant variance on transformed y

qqPlot(boxcox.fit)
lambda.errors<-boxcox.fit$residuals
lambda.fittedvalues<-boxcox.fit$fitted.values
plot(lambda.fittedvalues, lambda.errors, xlab="fitted y (^0.26)", ylab="lambda residuals")

lambda.abserrors<-abs(boxcox.fit$residuals)
plot(lambda.fittedvalues, lambda.abserrors)


#collinearity: ridge regression


# Prepare the data
x <- as.matrix(trainingdata[, c("AGE", "FEMALE", "SLEEP", "SNORE", "WEIGHT", "BMI", "WAIST")])
y <- trainingdata$vat.lambda

# Run Ridge Regression
ridge_model <- cv.glmnet(X, y, alpha = 0, lambda = seq(0.001, 1, by = 0.001))

# Get the best lambda value selected by cross-validation
best_lambda <- ridge_model$lambda.min

# Fit the final Ridge Regression model using the best lambda
final_ridge <- glmnet(X, y, alpha = 0, lambda = best_lambda)

#ridge regression coefficients

coefficients = final_ridge$beta


#potential models
#box cox:  boxcox.fit
#ridge  model: final_ridge


#checking for outlying X observations

plot(hatvalues(boxcox.fit))
plot(hatvalues(final_ridge))



#calculating errors, sse, rsq, adj rsq for ridge model

n<-1511
p<-8
training.vatlambda <- trainingdata$vat.lambda
x <- as.matrix(trainingdata[, c("AGE", "FEMALE", "SLEEP", "SNORE", "WEIGHT", "BMI", "WAIST")])
ridge.predicted <- final_ridge %>% predict(x)
ridge.errors<- training.vatlambda - ridgefit
ridge.sse <- sum((training.vatlambda - ridgefit)**2)
ridge.mse <- ridge.sse/(n-p)
ridge.ssr <- sum((ridgefit - mean(training.vatlambda))^2)
ridge.msr <- ridge.ssr/(p-1)
ridge.ssto <- ridge.sse + ridge.ssr
ridge.rsq <- 1 - (ridge.sse/ridge.ssto)
ridge.rsq.adj <- 1 - ((1-ridge.rsq)*(n-1))/(n-p-1)


#calculating mspr for box box + ridge model

testing.vatlambda <- testingdata$vat.lambda
x.testing <- as.matrix(testingdata[, c("AGE", "FEMALE", "SLEEP", "SNORE", "WEIGHT", "BMI", "WAIST")])
testing.predictions <- final_ridge %>% predict(x.testing)
mspr.ridge <- sum((testing.vatlambda - testing.predictions)**2)/(200)

#calculating mspr for box-cox model

testing.vatlambda <- testingdata$vat.lambda
x.testing <- as.data.frame(testingdata[, c("AGE", "FEMALE", "SLEEP", "SNORE", "WEIGHT", "BMI", "WAIST")])
testing.predictions <- boxcox.fit %>% predict(x.testing)
mspr.boxcox <- sum((testing.vatlambda - testing.predictions)**2)/(200)








