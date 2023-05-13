setwd("C:/Users/Tathagata/OneDrive/Desktop/MGT_7177_A2/R_A2")
library(readxl)
data<- read_excel("banksv.xlsx")
library(tidyverse)
library(dplyr)
library(caret)
library(skimr)
library(ggplot2)
install.packages("psych", dependencies = T)
library(psych)
install.packages("car")
library(car)


#Data Exploration
View(data)
summary(data) #summarising data
skim(data)    # summarising data using skim function
qplot(mapping = aes(x=as.factor(data$subscribed),y=data$age),data,geom=c("boxplot","point"), xlab = "Subscibed", ylab = "Age (in years)", color = data$subscribed)
                                         #plotting the age for each subscription group
table(data$subscribed)

table(datacln$month)                       #tabulating all categorical variables to gain insight on the existing levels.
table(datacln$day_of_week)
table(datacln$default)
table(datacln$housing)
table(datacln$loan)
table(datacln$job)
table(datacln$contact)
table(datacln$previous)
table(datacln$poutcome)
table(datacln$subscribed)
table(datacln$cons.conf.idx)
table(datacln$marital)

#Data Cleaning
 
datacln <- data %>% replace(is.na(.),999)   #replacing na values with 999
datacln <- datacln %>% filter(age>18&age< 85) #removing age outliers from the dataset

datacln$month <- as.factor(datacln$month)  #factorising the categorical variables for ease of computation
levels(datacln$month)[7] <- "mar"

datacln$day_of_week <- as.factor(datacln$day_of_week)

datacln$default <- as.factor(datacln$default)
levels(datacln$default)[1] <- "no"

datacln$housing <- as.factor(datacln$housing)

datacln$loan <- as.factor(datacln$loan)

datacln$job <- as.factor(datacln$job)

datacln$contact <- as.factor(datacln$contact)

datacln$previous <- as.factor(datacln$previous)

datacln$poutcome <- as.factor(datacln$poutcome)

datacln$marital <- as.factor(datacln$marital)

datacln$subscribed <- as.factor(datacln$subscribed)

datacln$education <- as.factor(datacln$education)

datacln$default <- as.factor(datacln$default)



View(datacln)
str(datacln)
skim(datacln)

#visualisation of trends


  ggplot(data = datacln, mapping= aes(x=datacln$subscribed, y=datacln$emp.var.rate,color=datacln$subscribed))+ geom_boxplot() + labs(x="Subscribed", y = "employee variation rate ") + ggtitle("Subscribed and Employee Variation rate")
   
  ggplot(data = datacln, mapping= aes(x=datacln$subscribed,y=datacln$duration, color=datacln$subscribed)) + geom_boxplot() + facet_wrap(~default) + labs(x="Subscribed", y = "Duration (in seconds) ") + ggtitle("Duration and Subscribed, by marital status") 
  
  ggplot(data = datacln, mapping= aes(x=datacln$subscribed,y=datacln$age, color=datacln$subscribed)) + geom_boxplot() + facet_wrap(~marital)+ labs(x=" Subscribed", y = "Age(in years)") + ggtitle("Subscription by Age and marital status")

  ggplot(data = datacln, mapping= aes(y=datacln$duration, x=datacln$subscribed, fill=datacln$subscribed)) + geom_boxplot() + facet_wrap(~contact) +labs(x="Subscribed", y = "Duration (In seconds") + ggtitle("Duration and subscription by contact") 
 
  ggplot(data = datacln, mapping= aes(x=datacln$subscribed, fill=datacln$subscribed)) + stat_count(width = 0.5) + facet_wrap(~contact) +labs(x="Subscribed", y = "Count") + ggtitle("Subscription by contact") 
  
  ggplot(data = datacln, mapping= aes(x=datacln$age, fill=subscribed)) + stat_count(width=0.5) + facet_wrap(~default) + labs(x="Age (in years) ", y = "Count") + ggtitle("Age distribution by subscription and default")
  
  ##Hypotheses bivariate analysis.
  
  #H1 -- Substantial diff in mean of duration between y&n customers
  describeBy(datacln$duration, group = datacln$subscribed)
  t.test(duration ~ subscribed, data = datacln, conf.level= 0.95, na.action=na.exclude)  #performing t-test

  #H2 -- High correlation between customers having a housing loan and y&n term deposit.
  
  table(datacln$housing,datacln$subscribed)
  chisq.test(datacln$housing,datacln$subscribed, correct = F)     #performing chi-squared test
  pchisq(5.9126,1,lower.tail = F)

  #H3 -- strong relation between default and subscription
  
  table(datacln$default,datacln$subscribed)
  chisq.test(datacln$default,datacln$subscribed, correct = F)    #performing chi-squared test
  pchisq(412.84,1,lower.tail = F)

  #H4 -- Strong relationship between age and subscription
  describeBy(datacln$age, group = datacln$subscribed)
  t.test(age ~ subscribed, data = datacln, conf.level= 0.95, na.action=na.exclude)   #performing t-test

  
  #H5 -- Significant difference in mean euribor3m between the customers who subscribed and who didnot.
  describeBy(datacln$euribor3m, group = datacln$subscribed)
  t.test(euribor3m ~ subscribed, data = datacln, conf.level= 0.95, na.action=na.exclude)    #performing t-test
  
  
  ##Partitioning the data
  set.seed(40379173)
  index <- createDataPartition(datacln$subscribed,p=0.8,list=F)
  train <- datacln[index,]
  test <- datacln[-index,]
  
  logisticPseudoR2s <- function(LogModel) {                           #preparing function for calculating pseudoR2 for models
    dev <- LogModel$deviance 
    nullDev <- LogModel$null.deviance 
    modelN <- length(LogModel$fitted.values)
    R.l <-  1 -  dev / nullDev
    R.cs <- 1- exp ( -(nullDev - dev) / modelN)
    R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
    cat("Pseudo R^2 for logistic regression\n")
    cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
    cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
    cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
  }
  
  ##model1
  fm1 <- subscribed ~ duration + age + default
  model1 <- glm(fm1, data = train, family="binomial")  
  summary(model1)  
  logisticPseudoR2s(model1)               #calculating pseudoR2 for model
  exp(model1$coefficients)              #finding odd ratio of predictors
   logLik(model1)                     #calculating log likelihood
   devst1 <- -2 * logLik(model1)        #calculating deviance statistics
  vif(model1)                         #calculating variance inflation factor
  mean(vif(model1)) 
  plot(model1)                          #plotting the model to check for assumptions
  c1 <- cooks.distance(model1)        
  sum(c1>1)                          #checking for cook's distance
  
  train$Standardisedresiduals <- rstandard(model1)    #standardising residuals
  train$Studentisedresiduals <- rstudent(model1)  
  sum(train$Standardisedresiduals > 1.96)  
  
  prediction <- predict(model1, test, type="response")   #checking predictive accuracy of the model
  pred_data <- as.factor(ifelse(prediction > 0.5,"yes","no")) 
  postResample(pred_data,test$subscribed)    
  confusionMatrix(data = pred_data,test$subscribed)  
  
  cor(train$cons.price.idx,train$nr.employed)  
  
  ##model2
  fm2 <- subscribed ~ duration + age + cons.price.idx + euribor3m + pdays + marital 
  model2 <- glm(fm2, data = train, family="binomial")  
  summary(model2)  
  logisticPseudoR2s(model2)
  exp(model2$coefficients)  
  logLik(model2)
  devst2 <- -2 * logLik(model2)
  vif(model2)
  mean(vif(model2)) 
  plot(model2) 
  c1 <- cooks.distance(model2)
  sum(c1>1)
  
  train$Standardisedresiduals <- rstandard(model3)
  train$Studentisedresiduals <- rstudent(model3)  
  sum(train$Standardisedresiduals > 1.96)  
  
  prediction <- predict(model2, test, type="response")
  pred_data <- as.factor(ifelse(prediction > 0.5,"yes","no")) 
  postResample(pred_data,test$subscribed)    
  confusionMatrix(data = pred_data,test$subscribed)  
  
  cor(train$cons.price.idx,train$nr.employed)  
  
  
  ##model3
  fm3 <- subscribed ~ duration + age + cons.price.idx  + nr.employed + marital + contact + campaign + poutcome
  model3 <- glm(fm3, data = train, family="binomial")  
  summary(model3)  
  logisticPseudoR2s(model3)
  exp(model3$coefficients)  
  logLik(model3)
  devst3 <- -2 * logLik(model3)
  vif(model3)
  mean(vif(model3)) 
  plot(model2) 
  c1 <- cooks.distance(model1)
  sum(c1>1)
  
  train$Standardisedresiduals <- rstandard(model3)
  train$Studentisedresiduals <- rstudent(model3)  
  sum(train$Standardisedresiduals > 1.96)  
  
  prediction <- predict(model3, test, type="response")
  pred_data <- as.factor(ifelse(prediction > 0.5,"yes","no")) 
  postResample(pred_data,test$subscribed)    
  confusionMatrix(data = pred_data,test$subscribed)  

  cor(train$cons.price.idx,train$nr.employed)  
  
  ##Responses of all three models have been tabulated using the stargazer function
  install.packages("stargazer")
  library(stargazer)
  stargazer(model1,model2,model3,type="text",title="Model1: Results",align = T,out="model.html")
  
  
  