setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week5")
library(tm)
library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)

#############################  DETECTING VANDALISM ON WIKIPEDIA  ########################
#Problem 1.1 - Bags of Words
wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)

table(wiki$Vandal)[2]

#Problem 1.2 - Bags of Words
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

#Problem 1.3 - Bags of Words
#Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

#Problem 1.4 - Bags of Words
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#Repeat for removed words
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
dim(wordsRemoved)

#Problem 1.5 - Bags of Words
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords = cbind(wikiWords, wiki$Vandal)
colnames(wikiWords)[329] ="Vandal"

library(caTools)
set.seed(123)
spl = sample.split(wikiWords, SplitRatio = .7)
train = subset(wikiWords, spl == TRUE)
test = subset(wikiWords, spl == FALSE)

table(test$Vandal)[1]/sum(table(test$Vandal))
#0.5343643
str(train)

#Problem 1.6 - Bags of Words
library(rpart)
library(rpart.plot)
modelCART = rpart(Vandal ~., data=train, method="class")
pred = predict(modelCART, newdata = test)
pred[1:10, ]
pred.prob = pred[,2]
cm = table(test$Vandal, pred.prob >=0.5)
accuracy = sum(diag(cm))/sum(cm)
accuracy #[1] 0.5463918

#Problem 1.7 - Bags of Words
prp(modelCART)

#Problem 2.1 - Problem-specific Knowledge
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)[2]

#Problem 2.2 - Problem-Specific Knowledge
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCart2 = rpart(Vandal ~., data=wikiTrain2, method="class")
pred = predict(wikiCart2, newdata = wikiTest2, type="class")
cm = table(wikiTest2$Vandal, pred)
accuracy = sum(diag(cm))/sum(cm)
accuracy #[1] 0.5742489

#Problem 2.3 - Problem-Specific Knowledge
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

#Problem 2.4 - Problem-Specific Knowledge
wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)

wikiCart3 = rpart(Vandal ~., data=wikiTrain3, method="class")
pred = predict(wikiCart3, newdata = wikiTest3, type="class")
cm = table(wikiTest3$Vandal, pred)
accuracy = sum(diag(cm))/sum(cm)
accuracy #[1] 0.6572165

#Problem 3.1 - Using Non-Textual Data
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)

wikiCart4 = rpart(Vandal ~., data=wikiTrain4, method="class")
pred = predict(wikiCart4, wikiTest4, type="class")
cm= table(wikiTest4$Vandal, pred)
accuracy = sum(diag(cm))/sum(cm)
accuracy #[1] 0.7139175

#Problem 3.2 - Using Non-Textual Data
prp(wikiCart4)


###########  AUTOMATING REVIEWS IN MEDICINE  #########

#Problem 1.1 - Loading the Data
trials <- read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
summary(trials)
str(trials)
max(nchar(trials$abstract))

#Problem 1.2 - Loading the Data
dim(subset(trials, nchar(abstract)==0))[1]

#Problem 1.3 - Loading the Data
which.min(nchar(trials$title))
trials$title[which.min(nchar(trials$title))]

#Problem 2.1 - Preparing the Corpus

corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract =Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

  #Build document Matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

  #Sparness of at most 95%
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, .95)

  #As DataFrame
dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

dim(dtmTitle);dim(dtmAbstract)

#Problem 2.2 - Preparing the Corpus
# Abstract have more words

#Problem 2.3 - Preparing the Corpus
which.max(colSums(dtmAbstract))

#Problem 3.1 - Building a model
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#Problem 3.2 - Building a Model
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)

#Problem 3.3 - Building a Model
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio = .7)
train = subset(dtm, spl==TRUE)
test = subset(dtm, spl==FALSE)

Baseline = table(test$trial)
BaselineModel = max(Baseline)/sum(Baseline)

#Problem 3.4 - Building a Model

Model1 = rpart(trial~., train, method="class")
pred = predict(Model1, newdata = test, type="class")
cm = table(test$trial, pred)
accuracy = sum(diag(cm))/sum(cm)
accuracy

prp(Model1)

#Problem 3.5 - Building a Model
prediction = predict(Model1)
predtest = predict(Model1, newdata = test)

max(prediction[,2])
max(predtest[,2])

#Problem 3.7 - Building a Model
cm = table(train$trial, prediction[,2] >.5)
TN = cm[1,1]
TP = cm[2,2]
FN = cm[2,1]
FP = cm[1,2]
Sensitivity =  TP/(TP+FN)     #TP / Positives
Specificity =  TN/(TN + FP)   #TN / Negatives
Acc = (TP + TN)/sum(cm) 
Sensitivity
Specificity
Acc

#Problem 4.1 - Evaluating the model on the testing set
cm = table(test$trial, pred)
accuracy = sum(diag(cm))/sum(cm)
accuracy

#Problem 4.2 - Evaluating the Model on the Testing Set
library(ROCR)
pred = predict(Model1, newdata = test)
pred = prediction(pred[,2], test$trial)
as.numeric(performance(pred,"auc")@y.values)

#PART 5: DECISION-MAKER TRADEOFFS
#5.1 A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3. A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3.
#5.2 A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3. A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3.
#5.3 A false negative is more costly than a false positive; the decision maker should use a probability threshold less than 0.5 for the machine learning model.

#############################  SEPARATING SPAM FROM HAM  ########################

#Problem 1.1 - Loading the Dataset
emails <- read.csv("emails.csv", stringsAsFactors = FALSE)
nrow(emails)

#Problem 1.2 - Loading the Dataset
table(emails$spam)
table(emails$spam)[2]

#Problem 1.3 - Loading the Dataset
emails$text[1]

#Problem 1.4 - Loading the Dataset
#Yes

#Problem 1.5 - Loading the Dataset
max(nchar(emails$text))

#Problem 1.6 - Loading the Dataset
which.min(nchar(emails$text))

#Problem 2.1 - Preparing the Corpus
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dim(dtm)[2]

#Problem 2.2 - Preparing the Corpus
#Sparness of at most 95% (containing terms appearing in at least 5%)
spdtm = removeSparseTerms(dtm, 0.95)
dim(spdtm)[2]

#Problem 2.3 - Preparing the Corpus
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))

#Problem 2.4 - Preparing the Corpus
emailsSparse$spam = emails$spam
sort(colSums(subset(emailsSparse, spam==0)))

#Problem 2.5 - Preparing the Corpus
sort(colSums(subset(emailsSparse, spam==1)))

# Problem 2.6 - Preparing the Corpus
# The frequencies of these most common words are likely to help differentiate between spam and ham. The frequencies of these most common words are likely to help differentiate between spam and ham. - correct

#Problem 2.7 - Preparing the Corpus
#The models we build are personalized, and would need to be further tested before being used as a spam filter for another person. 

#Problem 3.1 - Building machine learning models
emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = .7)
train = subset(emailsSparse, spl == T)
test = subset(emailsSparse, spl==F)

modelGLM = glm(spam~. , train, family = "binomial")
modelCART = rpart(spam~., train, method="class")
set.seed(123)
modelRF=randomForest(spam ~ . , data=train)

predGLM = predict(modelGLM, type="response")
predCART = predict(modelCART)[,2]
predRF = predict(modelRF, type="prob")[,2]

table(predGLM < 0.00001)
table(predCART > .99999)
table(predGLM >= .00001 & predGLM <= .9999 )

#Problem 3.2 - Building Machine Learning Models
summary(modelGLM)
#NONE below P value .05

#Problem 3.3 - Building Machine Learning Models
prp(modelCART)

#Problem 3.4 - Building Machine Learning Models
cm = table(train$spam, predGLM >.5)
sum(diag(cm))/sum(cm) #acc

#Problem 3.5 - Building Machine Learning Models
#AUC
library(ROCR)
prediction = prediction(predGLM, train$spam)
as.numeric(performance(prediction,"auc")@y.values)

#Problem 3.6 - Building Machine Learning Models
cm = table(train$spam, predCART >.5)
sum(diag(cm))/sum(cm) #acc

#Problem 3.7 - Building Machine Learning Models
prediction = prediction(predCART, train$spam)
as.numeric(performance(prediction,"auc")@y.values)

#Problem 3.8 - Building Machine Learning Models
cm = table(train$spam, predRF >.5)
sum(diag(cm))/sum(cm) #acc

#Problem 3.9 - Building Machine Learning Models
prediction = prediction(predRF, train$spam)
as.numeric(performance(prediction,"auc")@y.values)

#Problem 3.10 - Building Machine Learning Models
#Best model at training scenario GLM

#Problem 4.1 - Evaluating on the Test Set
predGLM = predict(modelGLM, test,type="response")
predCART = predict(modelCART,test)[,2]
predRF = predict(modelRF, test ,type="prob")[,2]

cm=table(test$spam, predGLM>.5)
sum(diag(cm))/sum(cm) #acc logregression

prediction = prediction(predGLM, test$spam)
as.numeric(performance(prediction,"auc")@y.values)

#Problem 4.3 - Evaluating on the Test Set
cm=table(test$spam, predCART>.5)
sum(diag(cm))/sum(cm) #acc 

prediction = prediction(predCART, test$spam)
as.numeric(performance(prediction,"auc")@y.values)

#Problem 4.5 - Evaluating on the Test Set
cm=table(test$spam, predRF>.5)
sum(diag(cm))/sum(cm) #acc logregression

prediction = prediction(predRF, test$spam)
as.numeric(performance(prediction,"auc")@y.values)

#Problem 4.7 - Evaluating on the Test Set
#Best on testing set Random Forest

################# SEPARATING SPAM FROM HAM (PART 2 - OPTIONAL) ################
#Problem 5.1 - Assigning weights to different types of errors
# False Negative
  #A spam email will be displayed in the main inbox, a nuisance for the email user. 

# False Positive
  #A false positive means the model labels a ham email as spam. This results in a ham email being sent to the Junk Email folder.

#Problem 5.2 - Assigning Weights to Different Types of Errors
  #A false negative is largely a nuisance (the user will need to delete the unsolicited email). However a false positive can be very costly, since the user might completely miss an important email due to it being delivered to the spam folder. Therefore, the false positive is more costly

#Problem 5.3 - Assigning Weights to Different Types of Errors
  #A false negative results in spam reaching a user's main inbox, which is a nuisance. A user who is particularly annoyed by such spam would assign a particularly high cost to a false negative.

#Problem 5.4 - Assigning Weights to Different Types of Errors
  #A false positive results in ham being sent to a user's Junk Email folder. While the user might catch the mistake upon checking the Junk Email folder, users who never check this folder will miss the email, incurring a particularly high cost.

#Problem 5.5 - Assigning Weights to Different Types of Errors
  #While before many users would completely miss a ham email labeled as spam (false positive), now users will not miss an email after this sort of mistake. As a result, the cost of a false positive has been decreased.

#Problem 5.6 - Assigning Weights to Different Types of Errors
  #Automatically collect information about how often each user accesses his/her Junk Email folder to infer preferences Automatically collect information about how often each user accesses his/her Junk Email folder to infer preferences - correct

#Problem 6.1 - Integrating Word Count Information
wordCount = rowSums(as.matrix(dtm))
  #wordCount would have only counted some of the words, but would have returned a result for all the emails

#Problem 6.2 - Integrating Word Count Information
hist(wordCount)
#The data is skew right -- there are a large number of small wordCount values and a small number of large values. 

#Problem 6.3 - Integrating Word Count Information
hist(log(wordCount))
  # The data is not skewed -- there are roughly the same number of unusually large and unusually small log(wordCount) values. The data is not skewed -- there are roughly the same number of unusually large and unusually small log(wordCount) values. - correct

#Problem 6.4 - Integrating Word Count Information
emailsSparse$logWordCount = log(wordCount)
boxplot(logWordCount~spam, emailsSparse)
  #logWordCount is slightly smaller in spam messages than in ham messages

#Problem 6.5 - Integrating Word Count Information
train2 = subset(emailsSparse, spl == T)
test2 = subset(emailsSparse, spl==F)

spam2CART <- rpart(spam ~., train2, method="class")
spam2RF=randomForest(spam ~ . , data=train2)

#Problem 6.6 - Integrating Word Count Information
predCART <-  predict(spam2CART, test2)[,2]
cm=table(test2$spam, predCART>.5)
sum(diag(cm))/sum(cm)

#Problem 6.7 - AUC
library(ROCR)
prediction = prediction(predCART, test2$spam)
as.numeric(performance(prediction,"auc")@y.values)

#Problem 6.8 - Integrating Word Count Information
predRF = predict(spam2RF, test2, type="prob")[,2]
cm=table(test2$spam, predRF>.5)
sum(diag(cm))/sum(cm)

#Problem 6.9 - AUC
prediction = prediction(predRF, test2$spam)
as.numeric(performance(prediction,"auc")@y.values)
