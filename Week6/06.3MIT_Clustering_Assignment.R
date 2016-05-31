setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week6")

#Homework ANALYTICS EDGE CLUSTERING

#############################  DOCUMENT CLUSTERING WITH DAILY KOS  ########################

#Problem 1.1 - Hierarchical Clustering
library(sparcl)
dailykos <- read.csv("dailykos.csv")
str(dailykos)

kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method = "ward.D")
# We have a lot of observations, so it takes a long time to compute the distance between each pair of observations.   We have a lot of variables, so the distance computation is long. 

#Problem 1.2 - Hierarchical Clustering
plot(kosHierClust)
#2 and 3

#Problem 1.3 - Hierarchical Clustering
#7 and 8

#Problem 1.4 - Hierarchical Clustering
clusterGroups = cutree(kosHierClust, k=7)
table(clusterGroups)

table(clusterGroups)[3]
which.max(table(clusterGroups))
which.min(table(clusterGroups))

#Problem 1.5 - Hierarchical Clustering
HierCluster1 = subset(dailykos, clusterGroups==1)
tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster1)))[6] #bush

#Problem 1.6 - Hierarchical Clustering
HierCluster2 = subset(dailykos, clusterGroups==2)
tail(sort(colMeans(HierCluster2)))

tail(sort(colMeans(subset(dailykos, clusterGroups==3))))
tail(sort(colMeans(subset(dailykos, clusterGroups==4))))
tail(sort(colMeans(subset(dailykos, clusterGroups==5))))
tail(sort(colMeans(subset(dailykos, clusterGroups==6))))
tail(sort(colMeans(subset(dailykos, clusterGroups==7))))
#cluster 5 and 7

#Problem 2.1 - K-Means Clustering
k=7
set.seed(1000)
KMC = kmeans(dailykos, centers = k)
str(KMC)

table(KMC$cluster) 
which.max(table(KMC$cluster))
which.min(table(KMC$cluster))

#Problem 2.2 - K-Means Clustering
for(i in 1:k){
  print(i)
  print(tail(sort(colMeans(subset(dailykos, KMC$cluster==i)))))
}

#Problem 2.3 - K-Means Clustering
table(clusterGroups,KMC$cluster) #Heirarchical is rows, and KMC the columns
which.max(table(clusterGroups,KMC$cluster)[,2])

#Problem 2.4 - K-Means Clustering
which.max(table(clusterGroups,KMC$cluster)[,3])

#Problem 2.5 - K-Means Clustering
which.max(table(clusterGroups,KMC$cluster)[,7])
# No Hierarchical Cluster contains at least half of the points in K-Means Cluster 7.

#Problem 2.5 - K-Means Clustering
which.max(table(clusterGroups,KMC$cluster)[,6])

#############################  MARKET SEGMENTATION FOR AIRLINES  ########################

#Problem 1.1 - Normalizing the Data
airlines <- read.csv("AirlinesCluster.csv")
summary(airlines)
#Problem 1.2 - Normalizing the Data

#Problem 1.3 - Normalizing the Data
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
#FlightMiles and DaysScince Enroll

#Problem 2.1 - Hierarchical Clustering
distances = dist(airlinesNorm, method = "euclidean")
HCAirlines = hclust(distances, method = "ward.D")
plot(HCAirlines) #6 clusters is not possible

#Problem 2.2 - Hierarchical Clustering
clusterGroups = cutree(HCAirlines, k=5)
table(clusterGroups)
table(clusterGroups)[1]


#Problem 2.3 - Hierarchical Clustering
for(i in 1:ncol(airlines)){
  print(names(airlines[i]))
  print(tapply(airlines[,i], clusterGroups, mean))
}

#Problem 2.4 - Hierarchical Clustering
  #Cluster2
  #Qual, FlightMiles, FlightTrans
  #Customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions. Customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions. - correct

#Problem 2.5 - Hierarchical Clustering
#cluster 3
for(i in 1:ncol(airlines)){
  print(names(airlines[i]))
  print(which.max(tapply(airlines[,i], clusterGroups, mean)))
}
#Balance, BonusMiles, Bonus Trans
#Miles through non flight transactions

#Problem 2.6 - Hierarchical Clustering
  #None
  #New customers accumulating miles through non flight transactions

#Problem 2.7 - Hierarchical Clustering
  #None
  #New customers accumulating miles not using the airline often

#Problem 3.1 - K-Means Clustering

k=5
set.seed(88)
KMAirlines = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(KMAirlines)

table(KMAirlines$cluster)
#2 clusters above 1000

#Problem 3.2 - K-Means Clustering
  #No, because cluster ordering is not meaningful in either k-means clustering or hierarchical clustering. 


#############################  DOCUMENT CLUSTERING WITH DAILY KOS  ########################
#Problem 1.1 - Exploring the Dataset
stocks <- read.csv("StocksCluster.csv")
str(stocks)
nrow(stocks)

#Problem 1.2 - Exploring the Dataset
table(stocks$PositiveDec)
table(stocks$PositiveDec)[2]/nrow(stocks)

#Problem 1.3 - Exploring the Dataset
cor(stocks[,1:11])

#Problem 1.4 - Exploring the Dataset
summary(stocks)
#April and September

#Problem 2.1 - Initial Logistic Regression Model
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data=stocksTrain,family=binomial)
prediction = predict(StocksModel, type="response")
fit.results = ifelse(prediction>.5,1,0) 
cm=table(stocksTrain$PositiveDec,fit.results )
acc= sum(diag(cm)/sum(cm))
acc

#Problem 2.2 - Initial Logistic Regression Model
prediction = predict(StocksModel, stocksTest, type="response")
fit.results = ifelse(prediction >.5,1,0) 
cm=table(stocksTest$PositiveDec,fit.results )
acc= sum(diag(cm)/sum(cm))
acc

#Problem 2.3 - Initial Logistic Regression Model
#Baseline
table(stocksTest$PositiveDec)
table(stocksTest$PositiveDec)[2]/nrow(stocksTest)


#Problem 3.1 - Clustering Stocks
#Remove labels
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology - correct

#Problem 3.2 - Clustering Stocks
library(caret)
#Normalize the data -> mean = 0 ; sd = 1
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

#Problem 3.3 - Clustering Stocks
summary(normTrain)
summary(normTest)
  #The distribution of the ReturnJan variable is different in the training and testing set The distribution of the ReturnJan variable is different in the training and testing set - correct

#Problem 3.4 - Clustering Stocks
set.seed(144)
km = kmeans(normTrain, 3)
table(km$cluster)
which.max(table(km$cluster))

#Problem 3.5 - Clustering Stocks
#flexcust to obtain training and testing set cluster assignments for our observations

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

#Problem 4.1 - Cluster-Specific Predictions
stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#Problem 4.2 - Cluster-Specific Predictions
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1,family=binomial)
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2,family=binomial)
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3,family=binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

#Problem 4.3 - Cluster-Specific Predictions
predict1 = predict(StocksModel1, stocksTest1, type="response")
predict2 = predict(StocksModel2, stocksTest2, type="response")
predict3 = predict(StocksModel3, stocksTest3, type="response")

sum(diag(table(stocksTest1$PositiveDec, predict1>.5)))/sum(nrow(stocksTest1))
sum(diag(table(stocksTest2$PositiveDec, predict2>.5)))/sum(nrow(stocksTest2))
sum(diag(table(stocksTest3$PositiveDec, predict3>.5)))/sum(nrow(stocksTest3))

#Problem 4.4 - Cluster-Specific Predictions
AllPredictions = c(predict1, predict2,predict3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

accuracy = sum(diag(table(AllOutcomes, AllPredictions>.5)))/length(AllOutcomes)
accuracy

