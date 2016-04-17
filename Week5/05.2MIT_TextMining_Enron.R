#Unit 5.2 Text Mining _ ENRON
setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week5")

emails = read.csv("energy_bids.csv", stringsAsFactors = FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1]) #Better presentation of a long string.
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]

table(emails$responsive)  #The data set is unbalance, typical in predictive coding problems
# 0   1 
# 716 139 

####Packages
library(tm)


#Construct Corpus for machine learning algorithms
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]])

#Build the document-term matrix
dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm, 0.97) #Remove sparse terms that dont appear in at least 3% of the document
dtm
labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
str(labeledTerms)

#Split data
library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, spl==TRUE)
test = subset(labeledTerms, spl==FALSE)

#Build model and train
library(rpart)
library(rpart.plot)
emailCart = rpart(responsive ~ . , data= train, method = "class")
prp(emailCart)

#Evaluate model
pred = predict(emailCart, newdata = test)
pred[1:10, ]
pred.prob = pred[,2]
cm = table(test$responsive, pred.prob >=0.5)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[2,1]+cm[1,2])
accuracy #[1] 0.8560311

#Baseline
table(test$responsive)
215/(215+42) #[1] 0.8365759

####ROC Curve
library(ROCR)
predROCR = prediction(pred.prob,test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = TRUE) #The best cutoff depends on the cost assigned by the decision maker.
performance(predROCR, "auc")@y.values #Our model can differentiate between a randomly selected responsive and -non responsive document about [1] 0.7936323 of the time. 
