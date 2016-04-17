#Unit 3. Logistic Regression
install.packages("caTools")
library(caTools)

#setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week3")
quality <- read.csv("quality.csv")
str(quality)

table(quality$PoorCare) #See how many people received Poorcare

#Baseline model
98/131 #We ware trying to beat this!

#Split data into Training and Testing
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = .75)  #Splits intellegently 
split #True means trainingset, False means Testingset

qualityTrain = subset(quality, split==TRUE)
qualityTest = subset(quality, split==FALSE)
nrow(qualityTrain)
nrow(qualityTest)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)
#It is important to see AIC, it is equivalent to R squared in Linear Regression.. (only be compared between models with the same dataset.)

predictTrain = predict(QualityLog, type="response")
summary(predictTrain)

tapply(predictTrain, qualityTrain$PoorCare, mean)

QualityLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family= binomial)
summary(QualityLog2)
QualityLog2$coefficients[2]

#True positive Rate  or Recall   <-    Sensitivity =  TP/(TP+FN)
#True negative Rate  or Preision <-    Specificity =  TN/(TN + FP)

table(qualityTrain$PoorCare , predictTrain > 0.5)
Sensitivity = 10/ (10 +15)
Specificity = 70/ (70+4)
#Now lets try increasing the threshold
table(qualityTrain$PoorCare , predictTrain > 0.7)
Sensitivity = 8/ (8 +17)  #Went down
Specificity = 73/ (73+1)  #Increased

table(qualityTrain$PoorCare , predictTrain > 0.2)
Sensitivity = 16/ (16 +9)  #Increased
Specificity = 54/ (54+20)  #Decreased

#ROC Curve
install.packages("ROCR")
library(ROCR)

ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr","fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.2,1.7))

##### QuickQuestion
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc


############# The Framingham Heart Study#######

framingham = read.csv("framingham.csv")
str(framingham)
library(caTools)
library(ROCR)

#Split data into Training and Testing
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)  #Splits intellegently 
split #True means trainingset, False means Testingset

train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial )#By using "." it uses all of the features as ind. var.
summary(framinghamLog)

#Prediction
predictTest = predict(framinghamLog, type="response", newdata = test)
cm = table(test$TenYearCHD, predictTest > 0.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Accuracy = (1069+11)/sum(table(test$TenYearCHD, predictTest > 0.5))
Accuracy
Baseline = (1069 + 6)/sum(table(test$TenYearCHD, predictTest > 0.5))

ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred,"auc")@y.values)
#[1] 0.7421095 Which means that the model can differentiate between low risk patients and high risk patients pretty well.

Sensitivity =  TP/(TP+FN)     #TP / Positives
Specificity =  TN/(TN + FP)   #TN / Negatives
Sensitivity
Specificity


##########Election Forecasting: Predicting the Winner Before any Votes are Cast

polling = read.csv("PollingData.csv")
str(polling)
table(polling$Year)
summary(polling)   #There is missing data (NAs) Remove observations? Remove variable? Fill?

#Filling missing values with "Multiple Imputation"
install.packages("mice")
library(mice)

simple = polling[c("Rasmussen" ,"SurveyUSA","PropR", "DiffCount")]
summary(simple)
set.seed(144)

imputed = complete(mice(simple))
summary(imputed) #No more NA!!!!!

#Copy back to the original data.frame

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling )

Train = subset(polling,  Year ==2004 | Year ==  2008)
Test = subset(polling, Year==2012)
table(Train$Republican)
Baseline = table(Train$Republican)[2]/sum(table(Train$Republican)) #Republican

sign(20) #If pass a positive number it returns +1, negative number -1, 0 returns 0.

table(sign(Train$Rasmussen))
table(Train$Republican , sign(Train$Rasmussen))

#Consider the possibility of multicolinearity

cor(Train) #Doesntwork becaose of State

cor(Train[c(c("Rasmussen" ,"SurveyUSA","PropR", "DiffCount","Republican"))])
#MODEL1
mod1 =glm(Republican ~PropR, data=Train, family=binomial)
summary(mod1)

pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >=.5)

#MODEL2
mod2 =glm(Republican ~ SurveyUSA + DiffCount, data=Train, family=binomial)
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >=.5)

summary(mod2)

#Evaluation on testing set.

table(Test$Republican, sign(Test$Rasmussen)) #WE compare against this.

TestPrediction = predict(mod2, newdata = Test, type="response")
table(Test$Republican, TestPrediction >=.5)

subset(Test, TestPrediction >= 0.5 & Republican ==0)
