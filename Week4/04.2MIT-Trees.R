#Unit 4.2 Trees and Cross Validation
setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week4")

library(caTools)

#ClaimsData Dataset: Medicare and Medical Service

Claims = read.csv("ClaimsData.csv")
table(Claims$bucket2009)/nrow(Claims)

#Split dataset
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio =  0.6)
ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)

#QuickQuestion
mean(ClaimsTrain$age) #[1] 72.62285
mean(ClaimsTrain$diabetes) #[1] 0.3804871

#Baseline
tbl<- table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
#Baseline's accuracy
sum(diag(tbl))/nrow(ClaimsTest)

PenaltyMatrix = matrix(c(0:4,2,0:4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=TRUE, nrow=5)
PenaltyMatrix #Actual outcomes on the left, Predicted on top.

#Multiply Baseline matrix with Penalty Matrix for Penalty Error
as.matrix(tbl)*PenaltyMatrix
sum(as.matrix(tbl)*PenaltyMatrix)/nrow(ClaimsTest)

#####QuickQuestion#####
ClaimsTest$newbaseline = 1
tbl2 = table(ClaimsTest$bucket2009, ClaimsTest$newbaseline)
sum(diag(tbl2))/nrow(ClaimsTest) #Accuracy

#Multiply baseline with Penalty Matrix(or vector)
as.matrix(tbl2)*c(0,2,4,6,8)
sum(as.matrix(tbl2)*c(0,2,4,6,8))/nrow(ClaimsTest)
#####QuickQuestion#####

#### Predicting HealthCare Cost in R #######
library(rpart)
library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer +
                     copd + depression + diabetes + heart.failure +
                     ihd + kidney + osteoporosis + stroke + bucket2008 +
                     reimbursement2008, data = ClaimsTrain , method = "class", cp=0.00005)
#Cp value was selected with CV on training set.

#Print tree
prp(ClaimsTree)

#Prediction
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
tbl<-table(ClaimsTest$bucket2009, PredictTest)
accuracy = sum(diag(tbl))/nrow(ClaimsTest)
accuracy

#PenaltyError
as.matrix(tbl)*PenaltyMatrix
sum(as.matrix(tbl)*PenaltyMatrix)/nrow(ClaimsTest)

#######   Now lets try using 'loss' function  ########
#This way we try to minimize our penalty error.

ClaimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer +
                     copd + depression + diabetes + heart.failure +
                     ihd + kidney + osteoporosis + stroke + bucket2008 +
                     reimbursement2008, data = ClaimsTrain , method = "class",
                     parms = list(loss=PenaltyMatrix),cp=0.00005)

#Prediction and Accuracy
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
tbl<-table(ClaimsTest$bucket2009, PredictTest)
accuracy = sum(diag(tbl))/nrow(ClaimsTest)
accuracy

#PenaltyError
as.matrix(tbl)*PenaltyMatrix
sum(as.matrix(tbl)*PenaltyMatrix)/nrow(ClaimsTest)
