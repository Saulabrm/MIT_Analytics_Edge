setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week5")
library(tm)


###########  DETECTING VANDALISM ON WIKIPEDIA  #########
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
