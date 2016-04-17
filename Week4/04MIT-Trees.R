#Unit 4. Trees and Cross Validation
setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week4")

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

stevens = read.csv("stevens.csv")
str(stevens)


set.seed(3000)

###TREES

spl = sample.split(stevens$Reverse, SplitRatio = 0.7)

train = subset(stevens, spl == TRUE)
test = subset(stevens, spl == FALSE)

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                    data = train,
                    method = "class",
                    minbucket = 25)
prp(StevensTree)

PredictCART = predict(StevensTree, newdata = test, type = "class") #This is like getting Threshold 0.5
cm = table(test$Reverse, PredictCART)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy

#ROCurve
library(ROCR)
PredictROC = predict(StevensTree, newdata = test)
PredictROC
pred = prediction(PredictROC[,2], test$Reverse) 
perf = performance(pred, "tpr", "fpr")
plot(perf)

#AUC
as.numeric(performance(pred,"auc")@y.values)

#New Model 
StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                    data = train,
                    method = "class",
                    minbucket = 5)
prp(StevensTree2)

#Minbucket = 100
StevensTree3 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                     data = train,
                     method = "class",
                     minbucket = 100)
prp(StevensTree3)

##RANDOM FOREST
set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue +Petitioner + Respondent + LowerCourt
                             + Unconst, data=train, nodesize=25, ntree = 200)
#Transform into factors
train$Reverse = as.factor(train$Reverse)
test$Reverse = as.factor(test$Reverse)

StevensForest = randomForest(Reverse ~ Circuit + Issue +Petitioner + Respondent + LowerCourt
                             + Unconst, data=train, nodesize=25, ntree = 200)
PredictForest = predict(StevensForest , newdata = test)
cm = table(test$Reverse, PredictForest)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy

###CROSS VALIDATION#######
library(caret)
library(e1071)

numFolds = trainControl(method = "cv", number = 10) #cv for cross validation, and 10 folds
cpGrid = expand.grid(.cp = seq(0.01,0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
      data=train, method="rpart" , trControl = numFolds, tuneGrid= cpGrid)

StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                      data=train, method="class", cp=0.19)
PredictCV = predict(StevensTreeCV, newdata = test, type="class")
cm = table(test$Reverse,PredictCV)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy

prp(StevensTreeCV)
#QuickQuestion : 1 split!!!