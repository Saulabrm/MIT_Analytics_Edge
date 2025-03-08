#Unit 4 Assignment
setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week4")

gerber <- read.csv("gerber.csv")
str(gerber)

#Problem 1.1 - Exploration and Logistic Regression
table(gerber$voting)
table(gerber$voting)[2]/sum(table(gerber$voting))

#Problem 1.2 - Exploration and Logistic Regression
apply(gerber,2, mean)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

#Problem 1.3 - Exploration and Logistic Regression
model1 = glm(voting~ civicduty + hawthorne+self+neighbors, family = "binomial", gerber)
summary(model1)

#Problem 1.4 - Exploration and Logistic Regression
prediction <- predict(model1, type="response")
cm <- table(gerber$voting, prediction > 0.3)
Accuracy= sum(diag(cm))/sum(cm)
Accuracy

##Problem 1.4 - Exploration and Logistic Regression
cm <- table(gerber$voting, prediction > 0.5)
Accuracy= sum(diag(cm))/sum(cm)
Accuracy

#Problem 1.6 - Exploration and Logistic Regression
library(ROCR)
ROCRpred = prediction(prediction, gerber$voting)
ROCRperf = performance(ROCRpred, "tpr","fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.2,1.7))

auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#Problem 2.1 - Trees
library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

#Problem 2.2 - Trees
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#Problem 2.3 Trees
#.31

#Problem 2.4 - Trees
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex , data=gerber, cp=0.0)
prp(CARTmodel3)

#Problem 3.1 - Interaction Terms
CARTmodel4 = rpart(voting ~ control , data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
.34-.296638

#Problem 3.2 - Interaction Terms
CARTmodel5 = rpart(voting ~ control + sex , data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

#Problem 3.3 - Interaction Terms
model = glm(voting~ control+sex, family = "binomial", gerber)
summary(model)

#Problem 3.4 - Interaction Terms
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(model, newdata=Possibilities, type="response")
# ( (Man, Not Control), (Man, Control), (Woman, Not Control), (Woman, Control) ).
#        1         2         3         4 
#0.3462559 0.3024455 0.3337375 0.2908065 

abs(.290456-0.2908065)

#Problem 3.5 - Interaction Terms
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
#f a person is a woman and in the control group, the chance that she voted goes down. If a person is a woman and in the control group, the chance that she voted goes down. - correct

#Problem 3.6 - Interaction Terms
abs(predict(LogModel2, newdata=Possibilities, type="response")[4] -.290456)

#Problem 3.7 - Interaction Terms
#No, it could lead to overfitting

###############################
###############################

# LETTER RECOGNITION #

#Problem 1.1 - Predicting B or not B
letters = read.csv("letters_ABPR.csv")
letters$isB <- as.factor(letters$letter =="B")

library(caTools)
set.seed(1000)
spl <- sample.split(letters$isB, SplitRatio = .5)

train <- subset(letters, spl==TRUE) 
test <- subset(letters, spl==FALSE) 

#Baseline   (predicts the most frequent outcome, which is "not B")
table(test$isB)[1]/sum(table(test$isB))

#Problem 1.2 - Predicting B or not B with CART
CARTb <- rpart(isB~. -letter, data=train, method="class")
prediction = predict(CARTb, test, type = "class")
cm = table(test$isB, prediction)
acc = sum(diag(cm))/sum(cm)
acc

#Problem 1.3 - Predicting B or Not B with RandomForest
library(randomForest)
set.seed(1000)
RFb <- randomForest(isB~. -letter, data= train)
prediction = predict(RFb, test, type="class")
cm = table(test$isB, prediction)
acc = sum(diag(cm))/sum(cm)
acc

#Problem 2.1 - Predicting the letters A, B, P, R
letters$letter = as.factor( letters$letter )

set.seed(2000)
spl = sample.split(letters, SplitRatio = .5)
train = subset(letters, spl = TRUE)
test = subset(letters, spl=FALSE)

#Baseline (predict the most frequent class of all of the options.)
max(table(test$letter))/nrow(test)

#Problem 2.2 - Predicting the letters A, B, P, R (Multiclass TREE)

CARTletter <- rpart(letter~. -isB, data = train, method="class")
prediction <- predict(CARTletter, newdata = test, type="class")
cm <- table(test$letter, prediction)
cm
acc = sum(diag(cm))/nrow(test)
acc

#Problem 2.3 - Predicting the letters A, B, P, R (RandomForest)

set.seed(1000)
RFletter <- randomForest(letter~. -isB, data= train)
prediction = predict(RFletter, test, type="class")
cm = table(test$letter, prediction)
acc = sum(diag(cm))/nrow(test)
acc

###############################
###############################

# PREDICTING EARNINGS FROM CENSUS DATA #

#Problem 1.1 - A Logistic Regression Model
census <- read.csv("census.csv")

set.seed(2000)
spl <- sample.split(census$over50k, SplitRatio =.6 )
train <- subset(census, spl==TRUE)
test <- subset(census, spl==FALSE)

fitGLM <- glm(over50k~., train, family="binomial")
summary(fitGLM)

#Problem 1.2 - A Logistic Regression Model
prediction <- predict(fitGLM, test)
cm <- table(test$over50k, prediction>.5)
acc <- sum(diag(cm))/nrow(test)
acc

#Problem 1.3 - A Logistic Regression Model #Baseline Test
max(table(test$over50k))/sum(table(test$over50k))

#Problem 1.4 - A Logistic Regression Model
#ROCurve
library(ROCR)
pred = prediction(prediction, test$over50k) 
as.numeric(performance(pred,"auc")@y.values)

#Problem 2.1 - A CART Model
CARTfit1 = rpart(over50k~., data= train, method="class")
prp(CARTfit1) #4 splits

#Problem 2.2 - A CART Model
#Relationship

#Problem 2.3 - A CART Model
#Education and capital gain

#Problem 2.4 - A CART Model
prediction <- predict(CARTfit1, test, type="class")
cm=table(test$over50k, prediction)
acc= sum(diag(cm))/nrow(test)
acc

#Problem 2.5 - A CART Model
#ROCurve CART
library(ROCR)
prediction <- predict(CARTfit1, test)
pred = prediction(prediction[,2], test$over50k) 
as.numeric(performance(pred,"auc")@y.values)

perf = performance(pred, "tpr", "fpr")
plot(perf)
plot(perf, colorize = TRUE)
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.2,1.7))

#ROCurve GLM
library(ROCR)
prediction <- predict(fitGLM, test)
pred = prediction(prediction, test$over50k) 
as.numeric(performance(pred,"auc")@y.values)

perf = performance(pred, "tpr", "fpr")
plot(perf)
plot(perf, colorize = TRUE)
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.2,1.7))


#Problem 3.1 - A Random Forest Model
set.seed(1)
trainSmall <- train[sample(nrow(train),2000),]
set.seed(1)
RFfit1 <- randomForest(over50k~.,train )
prediction <- predict(RFfit1, test)
cm = table(test$over50k, prediction)
acc= sum(diag(cm))/nrow(test)
acc

#Problem 3.2 - A Random Forest Model ************
vu = varUsed(RFfit1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(RFfit1$forest$xlevels[vusorted$ix]))
#chart that for each variable measures the number of times that variable was selected for splitting (the value on the x-axis)

#Problem 3.3 - A Random Forest Model
#Impurity reduction average
varImpPlot(RFfit1)

#Problem 4.1 - Selecting cp by Cross-Validation
library(caret)
set.seed(2)
#Specify the k folds
fitControl = trainControl(method = "cv", number = 10)
#Grid of cp values to evaluate
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k~., data=train, method="rpart", trControl=fitControl, tuneGrid=cartGrid)
#cp=0.002  acc=0.8515658

#Problem 4.2 - Selecting cp by Cross-Validation
CARTfit2 = rpart(over50k~., data= train, method="class", cp=.002)
prediction = predict(CARTfit2, test, type="class")
cm <- table(test$over50k, prediction)
acc <- sum(diag(cm))/sum(cm)
acc

prp(CARTfit2)

#Problem 4.3 - Selecting cp by Cross-Validation
