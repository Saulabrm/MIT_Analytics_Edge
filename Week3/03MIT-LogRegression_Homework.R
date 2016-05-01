setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week3")


############ 1 - POPULARITY OF MUSIC RECORDS ############

#Problem 1.1 - Understanding the Data
songs <- read.csv("songs.csv")
nrow(songs[songs$year==2010,])

#Problem 1.2 - Understanding the Data
nrow(songs[songs$artistname=="Michael Jackson",])

#Problem 1.3 - Understanding the Data
songs[songs$artistname=="Michael Jackson" & songs$Top10=="1",]

#Problem 1.4 - Understanding the Data
table(songs$timesignature)

#Problem 1.5 - Understanding the Data
songs[which.max(songs$tempo),2]

#Problem 2.1 - Creating Our Prediction Model
SongsTrain <- subset(songs, year<= 2009)
SongsTest <-  subset(songs, year==2010)

nrow(SongsTrain)

#Problem 2.2 - Creating our Prediction Model
nonvars <- c("year","songtitle", "artistname", "songID", "artistID")

SongsTrain <- SongsTrain[ ,!(names(SongsTrain) %in% nonvars)]
SongsTest <- SongsTest[ ,!(names(SongsTest) %in% nonvars)]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)$aic

#Problem 2.3 - Creating Our Prediction Model
#Higher because coeficients are positive

#Problem 2.4 - Creating Our Prediction Model
#Mainstream listeners tend to prefer less complex songs Mainstream listeners tend to prefer less complex songs - correct

#Problem 2.5 - Creating Our Prediction Model
  #Mainstream listeners prefer songs with heavy instrumentation Mainstream listeners prefer songs with heavy instrumentation - correct
  #NO

#Problem 3.1 - Beware of Multicollinearity Issues!
cor(SongsTrain$loudness,SongsTrain$energy)

#Problem 3.2 - Beware of Multicollinearity Issues!
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
  #Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1. Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1. - correct

#Problem 3.3 - Beware of Multicollinearity Issues!
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)
  #Yes

#Problem 4.1 - Validating Our Model
GLMprediction = predict(SongsLog3, type="response", newdata=SongsTest)

cm = table(SongsTest$Top10, GLMPrediction > .45)
accuracy = sum(diag(cm))/ sum(cm)
accuracy

#Problem 4.1 - Validating Our Model
#Baseline
table(SongsTest$Top10)
table(SongsTest$Top10)[1]/sum(table(SongsTest$Top10))

#Problem 4.3 - Validating Our Model
cm[2,2]
cm[1,2]

#Problem 4.4 - Validating Our Model
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Sensitivity =  TP/(TP+FN)     #TP / Positives
Specificity =  TN/(TN + FP)   #TN / Negatives
Sensitivity
Specificity
# Model 3 favors specificity over sensitivity.
# Model 3 provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely. So while it detects less than half of the Top 10 songs, we can be very confident in the songs that it does predict to be Top 10 hits.

############ 1 - POPULARITY OF MUSIC RECORDS ############
#########################################################


##########################################################
############ 2 - PREDICTING PAROLE VIOLATORS #############
parole <- read.csv("parole.csv")

#Problem 1.1 - Loading the Dataset
nrow(parole)

#Problem 1.2 - Loading the Dataset
table(parole$violator)[2]

#Problem 2.1 - Preparing the Dataset
summary(parole) 
  #state and crime

#Problem 2.2 - Preparing the Dataset
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole)
#Output like table()

#Problem 3.1 - Splitting into a Training and Testing Set
set.seed(144)
library(caTools)
split =  sample.split(parole$violator, SplitRatio = 0.7)

train <- subset(parole,split==TRUE)
test <- subset(parole,split==FALSE)

#70 and 30%

#Problem 3.2 - Splitting into a Training and Testing Set
#Exact if I run the 5 lines again, Different if I run them again without seed, Different if I use a different seed.

#Problem 4.1 - Building a Logistic Regression Model
model = glm(violator~. , train, family = "binomial")
summary(model)

#Problem 4.2 - Building a Logistic Regression Model
exp(1.61)

#Problem 4.3 - Building a Logistic Regression Model
#Odds
Odds=exp(-4.2411574 + 0.3869904 +0.8867192 + 50*-0.0001756 + 3*-0.1238867 +12*0.0802954+ 1*0.6837143)
Odds

#Probability
Probability = 1/(1+exp(1.700629)) 
  
test = rbind(test,c(1,1,50,1,3,12,0,2,0))
predict(model,test[203,],type="response")

#Problem 5.1 - Evaluating the Model on the Testing Set
prediction <- predict(model,test[-203,],type="response")
max(prediction)

#Problem 5.2 - Evaluating the Model on the Testing Set
cm = table(test$violator[-203], prediction > .5)

TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Sensitivity =  TP/(TP+FN)     #TP / Positives
Specificity =  TN/(TN + FP)   #TN / Negatives
Accuracy = sum(diag(cm))/sum(cm)
Sensitivity
Specificity
Accuracy

#Problem 5.3 - Evaluating the Model on the Testing Set
table(test$violator[-203])
179/202

#Problem 5.4 - Evaluating the Model on the Testing Set
table(test$violator[-203], prediction > .2)
#The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5. 

#Problem 5.5 - Evaluating the Model on the Testing Set
#The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value. The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value. - correct


#Problem 5.6 - Evaluating the Model on the Testing Set
library(ROCR)
ROCRpred = prediction(prediction, test$violator[-203])
ROCRperf = performance(ROCRpred, "tpr","fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.2,1.7))

auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#Problem 5.7 - Evaluating the Model on the Testing Set
#The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator.

#Problem 6.1 - Identifying Bias in Observational Data
# We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole or they completed their term.

############ 2 - PREDICTING PAROLE VIOLATORS #############
##########################################################



##########################################################
############ 3 - PREDICTING LOAN REPAYMENT   #############

#Problem 1.1 - Preparing the Dataset
loans <- read.csv("loans.csv")
str(loans)

table(loans$not.fully.paid)

table(loans$not.fully.paid)[2]/sum(table(loans$not.fully.paid))

#Problem 1.2 - Preparing the Dataset
colSums(is.na(loans))
colnames(loans)[colSums(is.na(loans)) > 0]

#Problem 1.3 - Preparing the Dataset
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
  #We want to be able to predict risk for all borrowers, instead of just the ones with all data reported.

#Problem 1.4 - Preparing the Dataset
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

#Problem 2.1 - Prediction Models
loansimputed <- read.csv("loans_imputed.csv")
set.seed(144)
split = sample.split(loansimputed$not.fully.paid, SplitRatio = 0.7)
train = subset(loansimputed, split == TRUE)
test = subset(loansimputed, split == FALSE)

loansGLM = glm(not.fully.paid~., family = "binomial", data = train)
summary(loansGLM)

#Problem 2.2 - Prediction Models
summary(loansGLM)$coef[13,1]
700*summary(loansGLM)$coef[13,1] - 710*summary(loansGLM)$coef[13,1]

exp(700*summary(loansGLM)$coef[13,1] - 710*summary(loansGLM)$coef[13,1])

#Problem 2.3 - Prediction Models
predicted.risk = predict(loansGLM, test, type="response")
cm = table(test$not.fully.paid, predicted.risk > .5)
Accuracy= sum(diag(cm))/sum(cm)
Accuracy

#Baseline
table(test$not.fully.paid)[1]/sum(table(test$not.fully.paid))

#Problem 2.4 - Prediction Models
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
ROCRperf = performance(ROCRpred, "tpr","fpr")
plot(ROCRperf)
plot(ROCRperf, colorize = TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-.2,1.7))

auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#Problem 3.1 - A "Smart Baseline"

model2 = glm(not.fully.paid ~ int.rate, family="binomial", train) 
summary(model2)
#int.rate is correlated with other risk-related variables, and therefore does not incrementally improve the model when those other variables are included.

#Problem 3.2 - A "Smart Baseline"
prediction = predict(model2, test, type="response")
max(prediction)

table(test$not.fully.paid, prediction > .5)
#0

#Problem 3.3 - A "Smart Baseline"
ROCRpred = prediction(prediction, test$not.fully.paid)
ROCRperf = performance(ROCRpred, "tpr","fpr")
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

#Problem 4.1 - Computing the Profitability of an Investment
10 * exp(.06*3)

#Problem 4.2 - Computing the Profitability of an Investment
  #c * exp(rt) - c c * exp(rt) - c - correct

#Problem 4.3 - Computing the Profitability of an Investment
  #-c

#Problem 5.1 - A Simple Investment Strategy
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
head(test$profit)

  #Maximum profit for a $10 dls investment
max(test$profit*10)

#Problem 6.1 - An Investment Strategy Based on Risk
sub <- subset(test, int.rate > .15)
mean(sub$profit*1)

table(sub$not.fully.paid)[2]/sum(table(sub$not.fully.paid))

#Problem 6.2 - An Investment Strategy Based on Risk
predicted.risk = predict(loansGLM, sub, type="response")
sub$predicted.risk = predicted.risk
cutoff = sort(sub$predicted.risk, decreasing=FALSE)[100]

selLoans = subset(sub, predicted.risk <= cutoff)
sum(selLoans$profit)


table(selLoans$not.fully.paid)[2]

############ 3 - PREDICTING LOAN REPAYMENT   #############
##########################################################

