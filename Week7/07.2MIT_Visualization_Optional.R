setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week7")

##### Visualizing Attributes of Parole Violators #####
library(ggplot2)


parole = read.csv("parole.csv")
str(parole)

#Problem 1.1 - Loading the Data
#Convert to factors
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

table(parole$male, parole$violator)
14/(64+14)

#Problem 1.2 - Loading the Data
#Kentucky = 2
#2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
which.max(table(parole$state, parole$crime)[2,])

#Problem 2.1 - Creating a Basic Histogram
ggplot(data = parole, aes(x = age)) + geom_histogram()
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) #20-24

#Problem 2.2 - Creating a Basic Histogram
#Add color, it changes the outline
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color="blue")
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, color="blue", fill="darkorange")

#Problem 3.1 - Adding Another Dimension
ggplot(data = parole, aes(x = age)) +
  geom_histogram(binwidth = 5, color="blue", fill="darkorange") +
  facet_grid(male~.)
#Most female paroles are from 35-39

#Problem 3.2 - Adding Another Dimension
#Histograms side by side .~male
ggplot(data = parole, aes(x = age)) +
  geom_histogram(binwidth = 5, color="blue", fill="darkorange") +
  facet_grid(.~male)

#Problem 3.3 - Adding Another DImension
colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill= male)) +
  geom_histogram(binwidth = 5) +
  scale_fill_manual(values=colorPalette)

#Trying with facet
ggplot(data = parole, aes(x = age, fill= male)) +
  geom_histogram(binwidth = 5, color= "white") +
  scale_fill_manual(values=colorPalette) +
  facet_grid(.~male)

#Reference: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

#Problem 3.4 - Adding Another Dimension
ggplot(data = parole, aes(x = age, fill= male)) +
  geom_histogram(binwidth = 5, color= "white", position = "identity", alpha = .5) +
  scale_fill_manual(values=colorPalette)

#Problem 4.1 - Time Served
ggplot(data = parole, aes(x=time.served)) +
  geom_histogram(binwidth = 1,color="white", fill="darkgreen")

#Problem 4.2 - Time Served
ggplot(data = parole, aes(x=time.served)) +
  geom_histogram(binwidth = 0.1,color="white", fill="darkgreen")
# Between 3.0 and 3.1 months

#Problem 4.3 - Time Served
ggplot(data = parole, aes(x=time.served)) +
  geom_histogram(binwidth = 1,color="white", fill="darkgreen") +
  facet_grid(.~crime)
#Driving has no time served less than one month
#Drug related

#Problem 4.4 - Time Served
ggplot(data = parole, aes(x=time.served, fill=crime)) +
  geom_histogram(binwidth = 1,color="white", position ="identity", alpha=.5) 
#With four different groups, it can be hard to tell them apart when they are overlayed. 

