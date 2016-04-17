#Unit 3. Logistic Regression
install.packages("caTools")
library(caTools)

setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week3")
baseball <- read.csv("baseball.csv")

# Problem 1.1 - Limiting to Teams Making the Playoffs
# Each row in the baseball dataset represents a team in a particular year.
# How many team/year pairs are there in the whole dataset?
nrow(baseball)
#[1] 1232

# Problem 1.2 - Limiting to Teams Making the Playoffs
# Though the dataset contains data from 1962 until 2012, we removed several years with shorter-than-usual seasons. Using the table() function, identify the total number of years included in this dataset.
table(baseball$Year)
# 47, Years (1972, 1981 1994, 1995) removed.

#Problem 1.3 - Limiting to Teams Making the Playoffs
#Because we're only analyzing teams that made the playoffs, use the subset() function to replace baseball with a data frame limited to teams that made the playoffs (so your subsetted data frame should still be called "baseball"). How many team/year pairs are included in the new dataset?

baseball <- subset(baseball, Playoffs == 1)
nrow(baseball)
#[1] 244

# Problem 1.4 - Limiting to Teams Making the Playoffs
# Through the years, different numbers of teams have been invited to the playoffs. Which of the following has been the number of teams making the playoffs in some season? Select all that apply.

table(baseball$Year)

#Problem 2.1 - Adding an Important Predictor
PlayoffTable= table(baseball$Year)
str(names(PlayoffTable))
#Vector of years stored as strings (type chr) 

#Problem 2.2 - Adding an Important Predictor
PlayoffTable[c("1990", "2001")]

#Problem 2.3 - Adding an Important Predictor
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

#Problem 2.4 - Adding an Important Predictor
str(baseball)
nrow(subset(baseball, NumCompetitors == 8))

#Problem 3.1 - Bivariate Models for Predicting World Series Winner
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)[1] #Teams that did not win the World Series

#Problem 3.2 - Bivariate Models for Predicting World Series Winner
LogReg<-c("Year",  "RS",  "RA", "W", "OBP", "SLG", "BA",  "RankSeason", "OOBP","OSLG" , "NumCompetitors", "League")


Fit1<- glm(WorldSeries ~ Year , data=baseball, family=binomial)
summary(Fit1)

Fit2<- glm(WorldSeries ~ RA , data=baseball, family=binomial)
summary(Fit2)

Fit3<- glm(WorldSeries ~ RankSeason , data=baseball, family=binomial)
summary(Fit3)

Fit4<- glm(WorldSeries ~ NumCompetitors , data=baseball, family=binomial)
summary(Fit4)

#Problem 4.1 - Multivariate Models for Predicting World Series Winner
Fit5<- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(Fit5)

#Problem 4.2 - Multivariate Models for Predicting World Series Winner
cor(baseball[c("Year","RA","RankSeason","NumCompetitors")])

#Problem 4.3 - Multivariate Models for Predicting World Series Winner
Fit5<- glm(WorldSeries ~ Year + RA, data = baseball, family = binomial)
summary(Fit5)
Fit6<- glm(WorldSeries ~ Year + RankSeason, data = baseball, family = binomial)
summary(Fit6)
Fit7<- glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(Fit7)
Fit8<- glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = binomial)
summary(Fit8)
Fit9<- glm(WorldSeries ~ RA + RankSeason, data = baseball, family = binomial)
summary(Fit9)
Fit10<- glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = binomial)
summary(Fit10)

#Min AIC for Fit4
