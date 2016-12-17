#' ---
#' output: pdf_document
#' ---
#' d---
#' title: "Project Final"
#' author: "Kelly Yang, Juan Guaracao, Alex Liu"
#' date: "5/14/2016"
#' output: html_document
#' ---
#' 
ddg.start("setup")
#knitr::opts_chunk$set(echo = FALSE)
library(Ecdat);
library(mosaic)
x <- DoctorAUS
ddg.finish("setup")
#' 
#' #Project Report
#' 
#' # 1. Introduction
#' 
#'   Access to and quality of healthcare is a primordial topic for all governments. In the last years, the United States has seen changes favoring free universal healthcare. 
#'     
ddg.start("test1")
x$sex2 <- "Male"
x$sex2[x$sex == "1"] <- "Female"
x$Age <- (x$age)*100
x$Income <- (x$income)*10000

ddg.finish("test1")
ddg.start("ddg.chunk_1")
histogram(~Age, data=x)
ddg.finish("ddg.chunk_1")
#' 
#'  Notice that the distribution appears bimodal, with peaks at 20-25 and 70+. This is due to the fact that the survey was limited to single individuals. The probability
#'  
ddg.start("ddg.chunk_2")
histogram(~illness, data=x) 
histogram(~doctorco, data=x)
ddg.finish("ddg.chunk_2")
#' 
#'  For the more moderate illness variable, we assumed a normal behavior (amid the limitations), in order to evaluate it using a linear regression model. On the contrary, for the more extreme cases, we transformed the variables into a binomial model.
#'  
ddg.start("ddg.chunk_3")
histogram(~doctorco, data=x)
x$doctorcobin <- 1
x$doctorcobin[x$doctorco == "0"] <- 0
histogram(~doctorcobin, data=x)
ddg.finish("ddg.chunk_3")
#'  
ddg.start("test2")
histogram(~doctorcobin, data=x)
ddg.finish("test2")
#' 
#'  Let us first consider our main predictor variable.
#'  
ddg.start("test3")
x$sex2 <- "Male"
x$sex2[x$sex == "1"] <- "Female"
x$Income <- (x$income)*100000
ddg.finish("test3")
#'  
ddg.start("ddg.chunk_4")
histogram(~Income, data=x)
ddg.finish("ddg.chunk_4")
#' 
#'  The distribution is the closest in the dataset to a normal distribution, allowing for the assumptions of normality to be satisfactorily fulfilled.
#'  
#' 
#' ## Income vs Age
#' 
ddg.start("ddg.chunk_5")
bwplot(Age~Income, data=x, ylab="Age groups (of five years)")
lmage<-lm(sqrt(income) ~ poly(age, 2, raw = T), data=x)

plot(x$age*100, sqrt(x$income), xlab="Age", ylab="Sqrt of Income, $10,000")
lines(x$age*100, predict(lmage))
ddg.finish("ddg.chunk_5")
#' 
#'  We first ploted the income versus the age of the subjects in our dataset.
#'  
#'  
#'  
ddg.start("ddg.chunk_6")
x$Age2 <- x$Age^2
Quadratic <- lm(Income ~ Age+Age2, data=x)
ddg.finish("ddg.chunk_6")
#' 
ddg.start("ddg.chunk_7")
summary(Quadratic)
ddg.finish("ddg.chunk_7")
#' 
#' From the quadratic model (and the R value) we see that age is a fair predictor of income, explaining part of income's variance with statistical significant results. More importantly, from the negative coefficient of Age2 (age squared), we can further validate the claim that, after a point, income decreases as one grows older.
#' 
#' ###Finding ILLNESS TO INCOME
ddg.start("ddg.chunk_8")
#data manipulation for binary outcome, 0 was no illness, 1 was illness
x$illnessbin <- 1
x$illnessbin[x$illness == 0] <- 0

#Logistic Regression
glmill <- glm(illnessbin ~ income + sex + age + chcond, data=x, family = "binomial")
summary(glmill)

#To arrive at those four specific variables, I used stepwise regression. First, I included all variables that would be the cause, not the result, of an illness. This excluded variables such as doctor consultations and medecines consumed, because those I guess are a result of one getting an illness. The hscore is measured using the Golberg method, which mainly analyzes a patient's psychiatric state, so I also didn't include it in the model, although when I did add it in it was statistically significant. 

x$logitillness <- predict(glmill)
#translate logit odd into probability
x$probabilityillness <- exp(x$logitillness)/(1+exp(x$logitillness))
#we found remarkable grouping of the data, and guessed that it was due to chronic conditions
xyplot(probabilityillness ~ income*10000, groups=chcond, auto.key=TRUE, data=x, type=c("p","r"), xlab="Income", ylab="Probability of Illness", main="Probability of Illness in Past 2 Weeks to Income")
ddg.finish("ddg.chunk_8")
#' 
#' #Appendix
#' 
#' ###Finding ILLNESS TO INCOME
## ----Illness Income, eval= FALSE-----------------------------------------
## #data manipulation for binary outcome, 0 was no illness, 1 was illness
## x$illnessbin <- 1
## x$illnessbin[x$illness == 0] <- 0
## 
## #Logistic Regression
## glmill <- glm(illnessbin ~ income + sex + age + chcond, data=x, family = "binomial")
## summary(glmill)
## 
## #To arrive at those four specific variables, I used stepwise regression. First, I included all variables that would be the cause, not the result, of an illness. This excluded variables such as doctor consultations and medecines consumed, because those I guess are a result of one getting an illness. The hscore is measured using the Golberg method, which mainly analyzes a patient's psychiatric state, so I also didn't include it in the model, although when I did add it in it was statistically significant.
## 
## x$logitillness <- predict(glmill)
## #translate logit odd into probability
## x$probabilityillness <- exp(x$logitillness)/(1+exp(x$logitillness))
## #we found remarkable grouping of the data, and guessed that it was due to chronic conditions
## xyplot(probabilityillness ~ income*10000, groups=chcond, auto.key=TRUE, data=x, type=c("p","r"), xlab="Income", ylab="Probability of Illness", main="Probability of Illness in Past 2 Weeks to Income")

#' 
#' 
