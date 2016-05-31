setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/KaggleComp")

library(dplyr)
library(caret)
library(randomForest)
library(party)
library(rpart)
library(e1071)
library(ROCR)

#LOAD DATA
train <- read.csv("train2016.csv")
test <- read.csv("test2016.csv")
sample <- read.csv("sampleSubmission2016.csv", stringsAsFactors = TRUE)
head(train)
summary(train)
str(train)

# Fix YOB
train$YOB[is.na(train$YOB)] = 1980
test$YOB[is.na(test$YOB)] = 1980


#Preprocess Missing values
# library(mice)
# set.seed(144)
# vars.for.imputation = setdiff(names(train), "Party")
# imputed = complete(mice(train[vars.for.imputation]))
# train[vars.for.imputation] = imputed

library(caTools)
set.seed(1)
spl <- sample.split(train$Party, SplitRatio = .7)
training <- subset(train, spl==TRUE) 
validating <- subset(train, spl==FALSE) 


#Models
rf <- randomForest(Party~.,data=training, ntree = 1000, na.action=na.roughfix)
PredictForest = predict(rf , newdata = validating)
cm=table(validating$Party, PredictForest)
sum(diag(cm))/sum(cm)


Rpart <- rpart(as.factor(Party) ~ .,data=training, method = "class")
PredictRpart = predict(Rpart, newdata = validating, type="class")
cm=table(validating$Party, prediction)

SVM <- svm( as.factor(Party) ~ ., training)
PredictSVM = predict(SVM, newdata= validating)
cm=table(validating$Party, PredictSVM)

GLM = glm( as.factor(Party) ~ YOB+Gender+EducationLevel ,  family = binomial(logit), data=training)
PredictGLM = predict(GLM, newdata= validating, type='response')
fitted.result = ifelse(PredictGLM > 0.3,"Democrat","Republican")
cm=table(validating$Party, fitted.result)

NB <- naiveBayes( as.factor(Party) ~ YOB+Gender, training)
PredictNB = predict(NB, newdata= validating)
cm=table(validating$Party, PredictNB)
sum(diag(cm))/sum(cm)



Predictions = cbind(as.data.frame(PredictForest),as.data.frame(PredictRpart), as.data.frame(PredictSVM),as.data.frame(fitted.result),as.data.frame(PredictNB))
names(Predictions) = c("RF","RPART","SVM", "GLM", "NB")

tail(Predictions)
head(as.numeric(Predictions[,1]))
Predictions$Ensemble = round((  2*as.numeric(Predictions[,1])+
                                1*as.numeric(Predictions[,2])+
                                1*as.numeric(Predictions[,3])+
                                0*as.numeric(Predictions[,4])+
                                0*as.numeric(Predictions[,5])
                              )/4)

for(i in 1:nrow(Predictions)){
  if(Predictions$Ensemble[i] == 1){
  Predictions$Ensemble[i] ="Democrat"
  }
  else{
  Predictions$Ensemble[i] = "Republican"
  }
} 

cm=table(validating$Party, Predictions[,6])
sum(diag(cm))/sum(cm)
#Best 2RF GLM(.4) NB



################  TESTING Currently BEST MODEL
#Accuracy : 0.65086 with 2RF, 1GLM, 1NB
#Models
rf <- randomForest(Party~.,data=train, ntree=500)
PredictForest = predict(rf , newdata = test)

Rpart <- rpart(as.factor(Party) ~ .,data=train, method = "class")
PredictRpart = predict(Rpart, newdata = test, type="class")

SVM <- svm( as.factor(Party) ~ ., train)
PredictSVM = predict(SVM, newdata= test)

GLM = glm( as.factor(Party) ~ YOB+Gender+EducationLevel ,  family = binomial(logit), data=train)
PredictGLM = predict(GLM, newdata= test, type='response')
fitted.result = ifelse(PredictGLM > 0.3,"Democrat","Republican")

NB <- naiveBayes( as.factor(Party) ~ YOB+Gender, train)
PredictNB = predict(NB, newdata= test)

Predictions = cbind(as.data.frame(PredictForest),as.data.frame(PredictRpart), as.data.frame(PredictSVM),as.data.frame(fitted.result),as.data.frame(PredictNB))
names(Predictions) = c("RF","RPART","SVM", "GLM", "NB")


Predictions$Ensemble = round((    2*as.numeric(Predictions[,1])+
                                  1*as.numeric(Predictions[,2])+
                                  1*as.numeric(Predictions[,3])+
                                  0*as.numeric(Predictions[,4])+
                                  0*as.numeric(Predictions[,5])
)/4)

for(i in 1:nrow(Predictions)){
  if(Predictions$Ensemble[i] == 1){
    Predictions$Ensemble[i] ="Democrat"
  }
  else{
    Predictions$Ensemble[i] = "Republican"
  }
} 


#Make them oposite
op<-data.frame()
op <- test %>% select(USER_ID) 
op <- cbind(op,Predictions$Ensemble)
names(op)<-c("USER_ID","Predictions")
head(op)

#Change
#p$Predictions[1200:1300]= Predictions$RPART[1200:1300]

#tail(op)


write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)



for(i in 1:700){
  if(op[i,2] == "Democrat")
  { op[i,2] = "Republican"}
  else
  { op[i,2] = "Democrat"}  
}

for(i in 900:1200){
  if(op[i,2] == "Democrat")
  { op[i,2] = "Republican"}
  else
  { op[i,2] = "Democrat"}  
}


