#MIT Analytics Edge Intro to R

#Q1
sd(c(5,8,12))
#[1] 3.511885

#Q2
which.min(c(4,1,6))
#[1] 2  Minimum is position 2 in the vector

#Q3
Sys.setlocale("LC_ALL","C")
#[1] "C/C/C/C/C/en_US.UTF-8"

#Explore R
8*6
2^16
sqrt(2)
abs(-65)
?sqrt
Squareroot2 = sqrt(2)
Squareroot2
HoursYear <- 365*24
HoursYear
ls()
#Vectors
c(2,3,5,8,13)
#Only one datatype per vector!!!!!
Country = c("Brazil","China","India","Switzerland","USA")
LifeExpectancy= c(74,76,65,83,79)
#Only one datatype per vector!!!!!
Country[1]
LifeExpectancy[3]
seq(0,100,2)
CountryData = data.frame(Country,LifeExpectancy)
CountryData
cbind(Country,LifeExpectancy) #Is a matrix, so it converts to a string.
CountryData$Population= c(199000,1390000,1240000,7997,318000)
CountryData
#Add Rows
Country = c("Australia", "Greece")
LifeExpectancy= c(81,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country, LifeExpectancy,Population)
NewCountryData
AllCountryData = rbind(CountryData,NewCountryData)
AllCountryData
setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week1")
list.files()

WHO=read.csv("WHO.csv")
str(WHO)
summary(WHO)
WHO_europe = subset(WHO, Region=="Europe")
str(WHO_europe)
write.csv(WHO_europe, "WHO_Europe.csv")
ls()
rm("WHO_europe")

WHO$Under15
mean(WHO$Under15) #Average of the population
sd(WHO$Under15)
summary(WHO$Under15) #1st Quartile, 25% of the data is below that value.
which.min(WHO$Under15) #Position of the minimum value for Under15
WHO$Country[86]  #[1] Japan has min population under 15
which.max(WHO$Under15)
WHO$Country[124] #[1] Niger has max population under 15
plot( WHO$GNI, WHO$FertilityRate)

Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)  # 7 countries of this subset
plot(Outliers$GNI, Outliers$FertilityRate)
Outliers[c("Country","GNI", "FertilityRate")]

mean(WHO$Over60)
#[1] 11.16366
which.min(WHO$Over60)
WHO$Country[which.min(WHO$Over60)]
# [1] United Arab Emirates

WHO$Country[which.max(WHO$LiteracyRate)] #Which Country has the highest literacy rate.
#[1] Cuba

#Histograms and Boxplots
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region) 
#Helps to understand the statistical range of a variable
#Medium line marks the median
#The box is 1st and 3rd quartile
#The extremes are the minumum and maximum exluding the outliers

boxplot(WHO$LifeExpectancy~WHO$Region, xlab="",ylab="Life Expectancy", main="Life Expectancy by Region")

table(WHO$Region) #Similar to summary, it tells the frequency of each different category.

tapply(WHO$Over60, WHO$Region, mean) #Splits the observations by region and then it gets the mean for population over 60
tapply(WHO$LiteracyRate, WHO$Region, min) #NA because there are missing values
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)

#Average Mortality of each region
tapply(WHO$ChildMortality, WHO$Region, mean)

USDA <- read.csv("USDA.csv")
str(USDA)
summary(USDA)
USDA$Sodium
which.max(USDA$Sodium)
names(USDA)
USDA$Description[which.max(USDA$Sodium)]

HighSodium = subset(USDA, Sodium > 10000)
nrow(HighSodium)
HighSodium$Description
match("CAVIAR", USDA$Description)
USDA$Sodium[match("CAVIAR", USDA$Description)] #compare it to the mean
summary(USDA$Sodium)
sd(USDA$Sodium)
sd(USDA$Sodium, na.rm=TRUE)

plot(USDA$Protein, USDA$TotalFat)
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab="Fat", main="Protein vs Fat", col="red", pch=19)
hist(USDA$VitaminC, xlab="Vitamin C (mg)", main="Histogram of Vitamin C levels")
hist(USDA$VitaminC, xlab="Vitamin C (mg)", main="Histogram of Vitamin C levels", xlim=c(0,100))
hist(USDA$VitaminC, xlab="Vitamin C (mg)", main="Histogram of Vitamin C levels", xlim=c(0,100), breaks=100)
hist(USDA$VitaminC, xlab="Vitamin C (mg)", main="Histogram of Vitamin C levels", xlim=c(0,100), breaks=2000)

boxplot(USDA$Sugar, main="Boxplot of Sugar Level", ylab="Sugar in grams")

USDA$Sodium[1] > mean(USDA$Sodium, na.rm=TRUE)
USDA$Sodium[50] > mean(USDA$Sodium, na.rm=TRUE)
HighSodium = USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE)
str(HighSodium)
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
str(USDA)
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))

table(USDA$HighSodium)
table(USDA$HighSodium,USDA$HighFat)
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)
#       0        1 
#2.558945 3.197294 
#Food with low Protein have mean of 2.55 of iron, and high protein have a mean of 3.19.
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)
