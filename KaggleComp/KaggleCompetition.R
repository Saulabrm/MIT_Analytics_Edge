setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/KaggleComp")

library(dplyr)
library(caret)
library(randomForest)
library(party)
library(rpart)
library(e1071)
library(ROCR)
registerDoMC(4)

#LOAD DATA
train <- read.csv("train2016.csv",na.strings=c("", "NA", "NULL"))
test <- read.csv("test2016.csv",na.strings=c("", "NA", "NULL"))
sample <- read.csv("sampleSubmission2016.csv", stringsAsFactors = TRUE)
head(train)
summary(train)
str(train)

#Join Datasets
full <- rbind(train[,-7],test)

# Fix YOB
full$YOB[full$YOB < 1910] = NA
full$YOB[full$YOB > 2010] = NA
# full$YOB[is.na(full$YOB)] = 1980
# full$YOB[full$YOB<1910 | full$YOB>2010] = 1980


### Treat Questions
  
#Get names of Questions and turn then numeric 
cols.num <- colnames(full[,7:107])
full[cols.num] <- sapply(full[cols.num], as.numeric)
#Change values to -1 for negative, 0 for NA, 1 for positive answers
full[cols.num] <- apply(full[cols.num], 2, function(x) {x[is.na(x)] <- 0; x})
full[cols.num] <- apply(full[cols.num], 2, function(x) {x[x == 1] <- -1; x})
full[cols.num] <- apply(full[cols.num], 2, function(x) {x[x == 2] <- 1; x})


#Preprocess Missing values
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(full), "Party")
imputed = complete(mice(full[vars.for.imputation]))
full[vars.for.imputation] = imputed


#Level Income
levelsI = c("under $25,000","$25,001 - $50,000","$50,000 - $74,999","$75,000 - $100,000","$100,001 - $150,000","over $150,000")
full$Income = ordered(full$Income, levels= levelsI)

# New Variable Social Class
SocialClass = as.character(full$Income)
SocialClass[SocialClass == "under $25,000" |SocialClass == "$25,001 - $50,000"] = "Low"
SocialClass[SocialClass == "$50,000 - $74,999" |SocialClass == "$75,000 - $100,000"] = "Medium"
SocialClass[SocialClass == "$100,001 - $150,000" |SocialClass == "over $150,000"] = "High"

full$SocialClass = as.factor(SocialClass)

# New Variable Age
# Age = full$YOB

#All Numeric
full <- as.data.frame(sapply(full, as.numeric))

#Clustering
hcdist<-dist(full)
hc<- hclust(hcdist,method="ward.D")
groups<-cutree(hc,5)
ColorDendrogram(hc, y = groups,  main = "My Dendograma", 
                branchlength = 80)

head(groups)

full$HC<-groups

#Kmeans
#Inertia
wss=c()
for (i in 1:20) {
  wss[[i]] <- kmeans(full,centers=i)$tot.withinss
}
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Kmeans
kc<-kmeans(full, 3)
full<-cbind(full,as.data.frame(kc[1])) #cluster



#Separate in train and test
Party = train$Party
train = full[1:nrow(train),]
train = cbind(train,Party)
test <- full[-(1:nrow(train)),]

#Correlations
train$Party <- as.numeric(train$Party) #1 for Democrat and -1 for Republican
train$Party[train$Party == 2] = -1  #Republican

correlation <- data.frame(102,2)
corr <- train[,c(7:107,109)]
for(i in 1:102){
  correlation[i,1] <- names(corr)[i]
  correlation[i,2] <- cor(corr[,i],corr[,102])
}
top5 <- correlation[order(correlation$X2, decreasing=TRUE),][2:6,1]
bottom5 <- correlation[order(correlation$X2, decreasing=FALSE),][1:5,1]
top5;bottom5

#Split Dataset
library(caTools)
set.seed(1)
spl <- sample.split(train$Party, SplitRatio = .7)
training <- subset(train, spl==TRUE) 
validating <- subset(train, spl==FALSE) 


features =c('USER_ID','YOB','Gender','Income','HouseholdStatus','EducationLevel','Party'
            ,'Q98197','Q109244','Q113181','Q115611','Q119851','Q105840',
            'Q120379','Q120472','Q120978','Q121011','Q98869','Q101163','Q99480','SocialClass' ,'cluster','HC')

newtraining = training[, (names(training) %in% features)]
newvalidating = validating[, (names(validating) %in% features)]
#newtest = testing[, (names(validating) %in% features)]

 #ModelCluster or kmeans
cluster = data.frame(3,2)
for(i in 1:5){
  newtraining=subset(training,Cluster==i)
  newvalidating=subset(validating,Cluster==i)
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  metric <- "Accuracy"
  # C5.0
  fit.c50 <- train(as.factor(Party)~ . -Cluster -USER_ID , data=newtraining, method="C5.0", metric=metric, trControl=control)
  PredictFit.c50 = predict(fit.c50 , newdata = newvalidating)
  cm=table(newvalidating$Party, PredictFit.c50)
  cluster[i,1]= i
  cluster[i,2]= sum(diag(cm))/sum(cm)
}



# Example of Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# C5.0
set.seed(7)
fit.c50 <- train(as.factor(Party)~ .  -USER_ID , data=newtraining, method="C5.0", metric=metric, trControl=control)
PredictFit.c50 = predict(fit.c50 , newdata = newvalidating)
cm=table(newvalidating$Party, PredictFit.c50)

# Stochastic Gradient Boosting
set.seed(7)
fit.gbm <- train(as.factor(Party)~ .  -USER_ID, data=newtraining, method="gbm", metric=metric, trControl=control, verbose=FALSE)
PredictGBM = predict(fit.gbm , newdata = newvalidating)
cm=table(newvalidating$Party, PredictGBM)

# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)



#Models
rf <- randomForest(as.factor(Party)~.,data=training, ntree = 500, na.action=na.roughfix)
PredictForest = predict(rf , newdata = validating)
cm=table(validating$Party, PredictForest)
sum(diag(cm))/sum(cm)

rffs <- cforest(as.factor(Party)~.,data=training)
PredictPartyForest = predict(rf , newdata = validating)
cm=table(validating$Party, PredictPartyForest)
sum(diag(cm))/sum(cm)


Rpart <- rpart(as.factor(Party) ~.,data=training, method = "class")
PredictRpart = predict(Rpart, newdata = validating, type="class")
cm=table(validating$Party, PredictRpart)

SVM <- svm( as.factor(Party) ~ .,training)
PredictSVM = predict(SVM, newdata= validating)
cm=table(validating$Party, PredictSVM)

GLM = glm( as.factor(Party) ~ . ,family = binomial(logit), data=training)
PredictGLM = predict(GLM, newdata= validating, type='response')
fitted.result = ifelse(PredictGLM > 0.5,"Republican","Democrat")
cm=table(validating$Party, fitted.result)

NB <- naiveBayes( as.factor(Party) ~., training)
PredictNB = predict(NB, newdata= validating)
cm=table(validating$Party, PredictNB)
sum(diag(cm))/sum(cm)



Predictions = cbind(as.data.frame(PredictForest),
                    as.data.frame(PredictPartyForest),
                    as.data.frame(PredictRpart), 
                    as.data.frame(PredictSVM),
                    as.data.frame(fitted.result),
                    as.data.frame(PredictNB),
                    as.data.frame(PredictFit.c50),
                    as.data.frame(PredictGBM)
                    )
names(Predictions) = c("RF","RF_FS", "RPART","SVM", "GLM", "NB","c50","GBM")

tail(Predictions)
head(as.numeric(Predictions[,1]))
Predictions$Ensemble = round((    1*as.numeric(Predictions[,1])+
                                  1*as.numeric(Predictions[,2])+
                                  0*as.numeric(Predictions[,3])+
                                  1*as.numeric(Predictions[,4])+
                                  1*as.numeric(Predictions[,5])+
                                  1*as.numeric(Predictions[,6])+
                                  1*as.numeric(Predictions[,7])+
                                  1*as.numeric(Predictions[,8])
)/7)




cor(sapply(Predictions, as.numeric))

for(i in 1:nrow(Predictions)){
  if(Predictions$Ensemble[i] == 1){
    Predictions$Ensemble[i] ="Democrat"
  }
  else{
    Predictions$Ensemble[i] = "Republican"
  }
} 

cm=table(validating$Party, Predictions[,9])
sum(diag(cm))/sum(cm)
#Best 2RF GLM(.4) NB


#### Make scripts
script.c50 = predict(fit.c50 , newdata = test)
script.c50 = as.character(script.c50)
script.c50[script.c50=="1"]="Democrat"
script.c50[script.c50=="-1"]="Republican"
script.c50 = as.factor(script.c50)

################  TESTING Currently BEST MODEL
#Accuracy : 0.65086 with 2RF, 1GLM, 1NB
#Models
rf <- randomForest(as.factor(Party)~.,data=train, ntree=1000)
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


Predictions$Ensemble = round((  2*as.numeric(Predictions[,1])+
                                  0*as.numeric(Predictions[,2])+
                                  0*as.numeric(Predictions[,3])+
                                  1*as.numeric(Predictions[,4])+
                                  1*as.numeric(Predictions[,5])
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
op <- cbind(op,script.c50)
names(op)<-c("USER_ID","Predictions")
head(op)

op$Predictions= Predictions$Ensemble

tail(op)


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


