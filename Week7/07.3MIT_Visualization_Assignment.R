setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week7")

####### VISUALIZATION ###########

##################  ELECTION FORECASTING REVISITED  ################## 
library(ggplot2)
library(maps)
library(ggmap)

#Problem 1.1 - Drawing a Map of the US
statesMap = map_data("state")
str(statesMap)
length(unique(statesMap$group))

#Problem 1.2 - Drawing a Map of the US
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

#Problem 2.1 - Coloring the States by Predictions
polling <- read.csv("PollingImputed.csv")

Train <- subset(polling,Year<2012)
Test <- subset(polling, Year==2012)

  #Model
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
  #Probability predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
  #Binary predictions 
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(TestPredictionBinary)
mean(TestPredictionBinary)

#Problem 2.2 - Coloring the States by Predictions

  #tolowercase to match with statesMap
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
  #merge  
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

predictionMap = predictionMap[order(predictionMap$order),]

nrow(predictionMap)
nrow(statesMap)

#Problem 2.3 - Coloring the States by Predictions
  # Because we only make predictions for 45 states, we no longer have observations for some of the states. These observations were removed in the merging process. Because we only make predictions for 45 states, we no longer have observations for some of the states. These observations were removed in the merging process. - correct

#Problem 2.4 - Coloring the States by Predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) +
  geom_polygon(color = "black")
  #Light blue are the republicans

#Problem 2.5 - Coloring the States by Predictions
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012") 
#colors()

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "gray", high = "black", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#Problem 3.1 - Understanding the Predictions
#Incorrectly

#Problem 3.2 - Understanding the Predictions
predictionDataFrame[predictionDataFrame$region=="florida",1]
  #Confident about a bad prediction

#PROBLEM 4 - PARAMETER SETTINGS
#Problem 4.1 - Parameter Settings
??geom_polygon
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black", linetype=3, size=3, alpha=.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012") 

#Problem 4.2 - Parameter Settings

#         # #         # #         # #         # 
#         # #         # #         # #         # 
################## VISUALIZING NETWORK DATA  ################## 
library(igraph)

# Problem 1.1 - Summarizing the 
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")
str(users)
str(edges)

(nrow(edges)*2)/nrow(users)

# Problem 1.2 - Summarizing the Data
table(users$locale)

# Problem 1.3 - Summarizing the Data
table(users$locale,users$gender)

# Problem 2.1 - Creating a Network
g = graph.data.frame(edges, FALSE, users)

# Problem 2.2 - Creating a Network
plot(g, vertex.size=5, vertex.label=NA)
  # 4 and 7

# Problem 2.3 - Creating a Network
count(degree(g) >= 10)

# Problem 2.4 - Creating a Network
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)

# Problem 3.1 - Coloring Vertices
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

# Problem 3.2 - Coloring Vertices
  #Color based on the school
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

# Problem 3.3 - Coloring Vertices
#Color based on the local users
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

# Problem 4 - Other Plotting Options


#         # #         # #         # #         # 
#         # #         # #         # #         # 
################## VISUALIZING TEXT DATA USING WORD CLOUDS ##################

# #Problem 1.1 - Preparing the Data
library(tm)
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))

dtm = DocumentTermMatrix(corpus)

allTweets <- as.data.frame(as.matrix(dtm))
head(allTweets)
ncol(allTweets)

# Problem 1.2 - Preparing the Data
  #It will be easier to read and understand the word cloud if it includes full words instead of just the word stems It will be easier to read and understand the word cloud if it includes full words instead of just the word stems 

# Problem 2.1 - Building a Word Cloud
library(wordcloud)
#colnames

# Problem 2.2 - Building a Word Cloud
  # ColSums

# Problem 2.3 - Building a Word Cloud
  wordcloud(colnames(allTweets),colSums(allTweets), scale = c(2,.025))

# Problem 2.4 - Building a Word Cloud
  tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
  corpus = Corpus(VectorSource(tweets$Tweet))
  corpus = tm_map(corpus,tolower)
  corpus = tm_map(corpus, PlainTextDocument)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
  dtm = DocumentTermMatrix(corpus)
  allTweets <- as.data.frame(as.matrix(dtm))
  
  wordcloud(colnames(allTweets),colSums(allTweets))
  
# PROBLEM 3 - SIZE AND COLOR
# Problem 3.1 - Size and Color
    #C
# Problem 3.2 - Size and Color
    #A
# Problem 3.3 - Size and Color
    #AD
# Problem 3.4 - Size and Color
    #A
# Problem 3.5 - Size and Color
    #D
  
#Problem 4.1 - Selecting a Color Palette
  library(RColorBrewer)
  brewer.pal()
  display.brewer.all()
  
#Problem 4.2 - Selecting a Color Palette
  #Greys
  
#Problem 4.3 - Selecting a Color Palette
  colors=brewer.pal(9, "Blues")
  brewer.pal(9, "Blues")[c(-1, -2, -3, -4)]
  brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)]
  