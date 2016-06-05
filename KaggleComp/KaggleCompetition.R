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

#Important Features
features =c('YOB','Age','AgeRange','Gender','Income','HouseholdStatus','EducationLevel',
                      'Q98197','Q109244','Q113181','Q115611','Q119851','Q105840',
                      'Q120379','Q120472','Q121011','Q101163','Q99480',
                      "Q123464", "Q121700","Q108617","Q99716","Q99581","Q119650","Q122120",
                      "Q107491", "Q98059","Q120650", "Q112512","Q115610","Q106993","Q122771","Q116601",
                      "Q99982","Q119334","Q114961","Q102687","Q113584",'cluster','HC', "SocialClass", "Party")

full = full[, (names(full) %in% features)]

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
 full$Age = as.numeric(format(Sys.Date(), "%Y")) - full$YOB 
 full$AgeRange = cut(full$Age, seq(0,90,15), right=FALSE, labels = c(0:5))
 
#All Numeric
full <- as.data.frame(sapply(full, as.numeric))

#Clustering
hcdist<-dist(full)
hc<- hclust(hcdist,method="ward.D")
groups<-cutree(hc,4)
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
kc<-kmeans(full, 5)
full<-cbind(full,as.data.frame(kc[1])) #cluster


#Separate in train and test
Party = train$Party
train = full[1:nrow(train),]
train = cbind(train,Party)
test <- full[-(1:nrow(train)),]


#Split Dataset
library(caTools)
set.seed(1)
spl <- sample.split(train$Party, SplitRatio = .8)
training <- subset(train, spl==TRUE) 
validating <- subset(train, spl==FALSE) 


features =c('YOB','AgeRange','Gender','Income','HouseholdStatus',
            'Q98197','Q109244','Q113181','Q115611','Q119851','Q105840',
            'Q120379','Q120472','Q121011','Q101163','Q99480',
             "Q121700","Q108617","Q122120",
            "Q107491", "Q98059","Q120650","Q115610","Q106993","Q122771","Q116601",
            "Q99982","Q102687","Q113584",'cluster',"Q99716", "SocialClass", "Party")

#108617, 98059     ,"Q99581","Q119334","Q114961" ,"Age" *GoodonCluster ,"Q123464","Q119650", "Q112512",'EducationLevel','HC'
newtraining = training[, (names(training) %in% features)]
newvalidating = validating[, (names(validating) %in% features)]

GLM = glm( as.factor(Party) ~ . ,family = binomial(logit), data=newtraining)
PredictGLM = predict(GLM, newdata= newvalidating, type='response')
fitted.result = ifelse(PredictGLM > 0.5,"Republican","Democrat")
cm=table(newvalidating$Party, fitted.result)
sum(diag(cm))/sum(cm)
summary(GLM)



 #ModelCluster or kmeans
cluster = data.frame(5,3)
for(i in 1:4){
  newtraining=subset(training,cluster==i)
  newvalidating=subset(validating,cluster==i)
  
  GLM = glm( as.factor(Party) ~ . ,family = binomial(logit), data=newtraining)
  PredictGLM = predict(GLM, newdata= newvalidating, type='response')
  fitted.result = ifelse(PredictGLM > 0.5,"Republican","Democrat")
  cm=table(newvalidating$Party, fitted.result)
  
  SVM <- svm( as.factor(Party) ~ .,newtraining)
  PredictSVM = predict(SVM, newdata= newvalidating)
  
  cluster[i,1]= i
  cluster[i,2]= sum(diag(cm))/sum(cm)
  cm=table(newvalidating$Party, PredictSVM)
  cluster[i,3]= sum(diag(cm))/sum(cm)
}
cluster

#  Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# C5.0
set.seed(7)
fit.c50 <- train(as.factor(Party)~ .  , data=newtraining, method="C5.0", metric=metric, trControl=control)
PredictFit.c50 = predict(fit.c50 , newdata = newvalidating)
cm=table(newvalidating$Party, PredictFit.c50)

# Stochastic Gradient Boosting
set.seed(7)
fit.gbm <- train(as.factor(Party)~ . , data=newtraining, method="gbm", metric=metric, trControl=control, verbose=FALSE)
PredictGBM = predict(fit.gbm , newdata = newvalidating)
cm=table(newvalidating$Party, PredictGBM)


# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)


#Models
rf <- randomForest(as.factor(Party)~.,data=newtraining, ntree = 200, na.action=na.roughfix)
PredictForest = predict(rf , newdata = newvalidating)
cm=table(newvalidating$Party, PredictForest)
sum(diag(cm))/sum(cm)

SVM <- svm( as.factor(Party) ~ .,newtraining)
PredictSVM = predict(SVM, newdata= newvalidating)
cm=table(newvalidating$Party, PredictSVM)

GLM = glm( as.factor(Party) ~ . ,family = binomial(logit), data=newtraining)
PredictGLM = predict(GLM, newdata= newvalidating, type='response')
fitted.result = ifelse(PredictGLM > 0.5,"Republican","Democrat")
cm=table(newvalidating$Party, fitted.result)
sum(diag(cm))/sum(cm)
summary(GLM)

NB <- naiveBayes( as.factor(Party) ~., newtraining)
PredictNB = predict(NB, newdata= newvalidating)
cm=table(newvalidating$Party, PredictNB)
sum(diag(cm))/sum(cm)


Predictions = cbind(as.data.frame(PredictForest),
                    as.data.frame(PredictSVM),
                    as.data.frame(fitted.result),
                    as.data.frame(PredictNB),
                    as.data.frame(PredictFit.c50),
                    as.data.frame(PredictGBM)
                    )
names(Predictions) = c("RF","SVM", "GLM", "NB","c50","GBM")


Predictions$Ensemble = round((    0*as.numeric(Predictions[,1])+
                                  1*as.numeric(Predictions[,2])+
                                  1*as.numeric(Predictions[,3])+
                                  0*as.numeric(Predictions[,4])+
                                  1*as.numeric(Predictions[,5])+
                                  1*as.numeric(Predictions[,6])
)/4)

#cor(sapply(Predictions, as.numeric))

for(i in 1:nrow(Predictions)){
  if(Predictions$Ensemble[i] == 1){
    Predictions$Ensemble[i] ="Democrat"
  }
  else{
    Predictions$Ensemble[i] = "Republican"
  }
} 

cm=table(newvalidating$Party, Predictions[,7])
sum(diag(cm))/sum(cm)
#Best 2RF GLM(.4) NB


################  TESTING Currently BEST MODEL
#Accuracy : 0.65086 with 2RF, 1GLM, 1NB

newtraining = train[, (names(train) %in% features)]
newvalidating = test[, (names(test) %in% features)]

#  Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
# C5.0
set.seed(7)
fit.c50 <- train(as.factor(Party)~ .  , data=newtraining, method="C5.0", metric=metric, trControl=control)
PredictFit.c50 = predict(fit.c50 , newdata = newvalidating)

# Stochastic Gradient Boosting
set.seed(7)
fit.gbm <- train(as.factor(Party)~ . , data=newtraining, method="gbm", metric=metric, trControl=control, verbose=FALSE)
PredictGBM = predict(fit.gbm , newdata = newvalidating)


# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)


#Models
rf <- randomForest(as.factor(Party)~.,data=newtraining, ntree = 200, na.action=na.roughfix)
PredictForest = predict(rf , newdata = newvalidating)

SVM <- svm( as.factor(Party) ~ .,newtraining)
PredictSVM = predict(SVM, newdata= newvalidating)

GLM = glm( as.factor(Party) ~ . ,family = binomial(logit), data=newtraining)
PredictGLM = predict(GLM, newdata= newvalidating, type='response')
fitted.result = ifelse(PredictGLM > 0.5,"Republican","Democrat")


NB <- naiveBayes( as.factor(Party) ~., newtraining)
PredictNB = predict(NB, newdata= newvalidating)



Predictions = cbind(as.data.frame(PredictForest),
                    as.data.frame(PredictSVM),
                    as.data.frame(fitted.result),
                    as.data.frame(PredictNB),
                    as.data.frame(PredictFit.c50),
                    as.data.frame(PredictGBM)
)
names(Predictions) = c("RF","SVM", "GLM", "NB","c50","GBM")


Predictions$Ensemble = round((    0*as.numeric(Predictions[,1])+
                                    1*as.numeric(Predictions[,2])+
                                    1*as.numeric(Predictions[,3])+
                                    0*as.numeric(Predictions[,4])+
                                    1*as.numeric(Predictions[,5])+
                                    1*as.numeric(Predictions[,6])
)/4)


for(i in 1:nrow(Predictions)){
  if(Predictions$Ensemble[i] == 1){
    Predictions$Ensemble[i] ="Democrat"
  }
  else{
    Predictions$Ensemble[i] = "Republican"
  }
} 


#Best 2RF GLM(.4) NB

#Make them oposite
op<-data.frame()
op <- test %>% select(USER_ID) 
op <- cbind(op,Predictions$Ensemble)
names(op)<-c("USER_ID","Predictions")


write.table(op,"output.csv", row.names=FALSE,sep=",",quote=F)




