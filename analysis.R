             ####### Section 2 #########
#Organisation of the code
      # 1. Logistic Reg. for Housing loan as DV
      # 2. Logistic Reg. for Term deposit subscription as DV
      # 3. CARTs for Housing loan as DV
      # 4. CARTs for Term deposit subscription as DV
             
             #Only model 1 and 4 are presented
#libraries
library(car)
library(psych)
library(lattice)
library(ggplot2)
library(verification)
library(sjPlot)
             
             
#Load the data
df=read.csv("Data2.csv")

#Let's inspect the data
head(df)
str(df)

#Let's clean the data, delete missing value and column we are not going to use
df$duration=NULL
df=df[!grepl("unknown", df$education),]
df=df[!grepl("unknown", df$job),]
df=df[!grepl("unknown", df$poutcome),]
df=df[!grepl("unknown", df$contact),]

#organise the data frame to only keep the variables needed for convinience 
dff=df
dff$day=NULL
dff$month=NULL
dff$campaign=NULL
dff$poutcome=NULL
dff$previous=NULL
dff$pdays=NULL
dff$y=NULL

dff$balance=as.numeric(dff$balance)

####### Model 1 : Logistic Reg. for Housing loan as DV ########
#Let's choose housing as the DV
dff$housing=ifelse(dff$housing == "yes", 1, 0)
dff$housing=as.factor(dff$housing)
dff$housing=relevel(dff$housing,"0") #set the baseline
levels(dff$housing)

#set the seed to be able to replicate the results
set.seed(999)

#we want the probability of a person would contract a housing loan 
library(caret)
Train=createDataPartition(dff$housing,p = .8,list = F)
trainSet=dff[Train,] #training set
testSet=dff[-Train,] #test set

#the model
fit=glm(housing~.,data=trainSet,family ="binomial" )
summary(fit)

#Let's check variance inflation factor
vif(fit)

#Pseudo R-squared
1-fit$deviance/fit$null.deviance

#BIC
fitStep=step(fit,k= log(nrow(trainSet))) 
summary(fitStep)
exp(fitStep$coefficients)

#Pseudo R-sqrt for housing ~ age + job + marital + education + balance + loan + contact
fit=glm(housing~age + job + marital + education + balance + loan + contact,data=trainSet,family ="binomial" )
1-fit$deviance/fit$null.deviance

#new Pseudo R-squared
1-fitStep$deviance/fitStep$null.deviance

#Bayes Factor: exp((model BIC -lowest BIC)/2)
exp((7342.21-7340.71)/2)
exp((7350.95-7340.71)/2)

#Performance on unseen data
pred=predict(fitStep,testSet, type='response')
head(pred)

#Let's see what is the best cut-off by examining the ROC curve
roc.plot(testSet$housing=='1', pred)

#Best cut-off 0.57
pred=ifelse(pred>0.57,1,0) #between Sensitivity=0.8 and specificity=0.5

#Confusion Matrix
print(confusionMatrix(as.factor(pred),as.factor(testSet$housing),positive = "1"))

#plot

ggplot(dff, aes(age, balance, color = housing)) + geom_point(alpha = 0.7) #before prediction

ggplot(testSet, aes(age, balance, color = pred)) + geom_point(alpha = 0.7) #After prediction 

fitResults = lm(pred~job, data = testSet) #Job
plot_model(fitResults,type='pred')

fitResults = lm(pred~marital, data = testSet) #Marital 
plot_model(fitResults,type='pred')

fitResults = lm(pred~education, data = testSet) #education
plot_model(fitResults,type='pred')

fitResults = lm(pred~loan, data = testSet) #loan
plot_model(fitResults,type='pred')


           ########## Model 4 : CARTs for Term deposit subscription #######

set.seed(3)
#Variable transformation
df$Y=ifelse(df$y == "yes", 1, 0)
#Delete categorical column y
df$y=NULL
df$Y=as.factor(df$Y)
df$Y=relevel(df$Y,"0") #baseline


#split the data
#Not that the new binary variable Y was created previously
library(caret)
inTrain = createDataPartition(df$Y, p = 0.8, list = F)
trainData = df[inTrain,]
testData = df[-inTrain,]
fit = train(Y~loan+campaign+day+housing+month+poutcome, data = trainData, method = "rpart")


#Let's plot this
library(rpart.plot)
rpart.plot(fit$finalModel, type = 2, fallen.leaves = F)

#take the prediction probabilities
pred = predict(fit, testData,type = "prob")
head(pred)
head(pred[,2]) #we want "yes" which corresponds to the second column 

#Best cut-off #Sensitivity=0.5 #specificity=0.1 
library(verification)
roc.plot(testData$Y == 1, pred[,2])

#Confusion Matric
predBins = ifelse(pred[,2] > 0.21, 1, 0)
print(confusionMatrix(as.factor(predBins),
                      as.factor(testData$Y), positive = "1"))

require(randomForest)
fit=randomForest(Y~., data = trainData)
varImpPlot(fit)

#Plot
pred2=pred[,2] 

fitResults = lm(pred2~day, data = testData) #days
plot_model(fitResults,type='pred')

fitResults = lm(pred2~month , data = testData) #months
plot_model(fitResults,type='pred')

######## Model 2 : Client Subscription Logistic Regressio n#####


#set seed
set.seed(999)

#Split the data 
Train=createDataPartition(df$Y,p=0.8,list=F)
trainSet=df[Train,]
testSet=df[-Train,]

#fit the model
fit=glm(Y~.,data=trainSet,family='binomial')
summary(fit)
vif(fit)

#Pseudo R-squared
1-fit$deviance/fit$null.deviance

#Best model
fitStep=step(fit,k=log(nrow(trainSet)))
summary(fitStep)          
exp(fitStep$coefficients)

#let's see how the model performs on the unseen data
pred=predict(fitStep, testSet, type = "response")
head(pred)

#Let's see what is the best cut-off by examining the ROC curve
roc.plot(testSet$Y=='1', pred)

#Best cut-off 0.32
pred=ifelse(pred>0.32,1,0) #Sensitivity=0.75 Specificity=0.2

#Confusion Matrix
print(confusionMatrix(as.factor(pred),as.factor(testSet$Y),positive = "1"))

#Assumptions 
#Dependant varianle is binary and ordinal 
#Observations are independant 
#Test for multicolinearity vif(fit)

###### CARTs Housing Loans  #### 

#split the data
inTrain = createDataPartition(dff$housing, p = 0.8, list = F)
trainData = dff[inTrain,]
testData = dff[-inTrain,]
fit = train(housing ~ ., data = trainData, method = "rpart")

#Let's plot this
library(rpart.plot)
rpart.plot(fit$finalModel,type = 2,fallen.leaves = T)

#take the prediction probabilities
pred = predict(fit, testData, type = "prob")
head(pred)

head(pred[,2])

#Best cut-off
roc.plot(testData$housing == '1', pred[,2])

#Confusion Matric
predBins = ifelse(pred[,2] > 0.59, 1, 0)
print(confusionMatrix(as.factor(predBins),
                      as.factor(testData$housing), positive = "1"))


#Let's take a look at the training model confusion matrix and OOB error
print(fit$finalModel)

#End 

