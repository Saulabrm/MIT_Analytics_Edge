#1.1 Homework AN ANALYTICAL DETECTIVE

setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week1")

#####################################################   
#############  ANALYTICAL DETECTIVE ########### 
mvt = read.csv("mvtWeek1.csv")
head(mvt)
#Problem 1.1 - Loading the Data : How many rows of data (observations) are in this dataset?
nrow(mvt)
#Problem 1.2 - Loading the Data : How many variables are in this dataset?
ncol(mvt)
# Problem 1.3 - Loading the Data:
# Using the "max" function, what is the maximum value of the variable "ID"?
max(mvt$ID)
summary(mvt)
# Problem 1.4 - Loading the Data
# What is the minimum value of the variable "Beat"?
min(mvt$Beat)
# Problem 1.5 - Loading the Data
# How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
table(mvt$Arrest)
# Problem 1.6 - Loading the Data
# How many observations have a LocationDescription value of ALLEY?
table(mvt$LocationDescription)

#Problem 2.1 - Understanding Dates in R
#[1]Month/Day/Year Hour:Minute Month/Day/Year Hour:Minute - correct

#Problem 2.2 - Understanding Dates in R
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
#[1] May 2006

#Problem 2.3 - Understanding Dates in R
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
min(table(mvt$Month)) #Sunday
#Problem 2.4 - Understanding Dates in R 
max(table(mvt$Weekday)) #friday
#Problem 2.5 - Understanding Dates in R
table(mvt$Month,mvt$Arrest) #January

#Problem 3.1 - Visualizing Crime Trends
mvt$Date = DateConvert
hist(mvt$Date, breaks=100)
#[1] Decrease Decrease Increase

#Problem 3.2 - Visualizing Crime Trends
boxplot(mvt$Date~mvt$Arrest)

#Problem 3.3 - Visualizing Crime Trends
table(mvt$Date,mvt$Arrest)
2152/(18517+2152)

#Problem 3.4 - Visualizing Crime Trends
table(mvt$Date,mvt$Arrest)
1212/(13068+1212)

#Problem 3.5 - Visualizing Crime Trends
550/(13542+550)

#Problem 4.1 - Popular Locations
sort(table(mvt$LocationDescription))
#Street, Parking, Alley, GasSt, Driveway

#Problem 4.2 - Popular Locations
top5 = subset(mvt, LocationDescription == "STREET" |
                LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
                LocationDescription == "ALLEY" |
                LocationDescription == "GAS STATION" |
                LocationDescription == "DRIVEWAY - RESIDENTIAL"
                )

#OR
Loc = c("STREET","PARKING LOT/GARAGE(NON.RESID.)","ALLEY","GAS STATION","DRIVEWAY - RESIDENTIAL")
top5= subset(mvt, LocationDescription %in% Loc)

nrow(top5)
#[1] 177510

#Problem 4.3 - Popular Locations
top5$LocationDescription = factor(top5$LocationDescription)
table(top5$LocationDescription, top5$Arrest)
table(top5$LocationDescription, top5$Arrest)[,2]/(table(top5$LocationDescription, top5$Arrest)[,1]+table(top5$LocationDescription, top5$Arrest)[,2])

#Problem 4.4 - Popular Locations
max(table(top5$Weekday, top5$LocationDescription)[,3])
#Saturday

#Problem 4.4 - Popular Locations
table(top5$Weekday, top5$LocationDescription)[,2]
min(table(top5$Weekday, top5$LocationDescription)[,2])
#############  ANALYTICAL DETECTIVE ########### 
############################################### 

#############  Stock Market ################### 
############################################### 

#READ DATA
IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv" )
CocaCola = read.csv("CocaColaStock.csv")
Boeing  = read.csv("BoeingStock.csv")

#Change Date Type
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#Problem 1.1 - Summary Statistics
nrow(IBM)
#[1] 480

#Problem 1.2 - Summary Statistics
summary(Boeing)
min(Boeing$Date)

#Problem 1.3 - Summary Statistics
max(Boeing$Date)

#Problem 1.4 - Summary Statistics
mean(IBM$StockPrice)

#Problem 1.5 - Summary Statistics
min(GE$StockPrice)

#Problem 1.6 - Summary Statistics
max(CocaCola$StockPrice)

#Problem 1.7 - Summary Statistics
summary(Boeing)

#Problem 1.8 - Summary Statistics
sd(ProcterGamble$StockPrice)

#Problem 2.1 - Visualizing Stock Dynamics
plot(CocaCola$Date,CocaCola$StockPrice)
#1973 and 1980

#Problem 2.2 - Visualizing Stock Dynamics
plot(CocaCola$Date,CocaCola$StockPrice, col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue", lty=2, lwd=2)
abline(v=as.Date(c("2000-03-01")), lwd=2)

#Procter and Gamble

#Problem 2.3 - Visualizing Stock Dynamics
abline(v=as.Date(c("1983-01-01")), lwd=2)
abline(v=as.Date(c("1984-01-01")), lwd=2)
#Cocacola and Cocacola

#Problem 3.1 - Visualizing Stock Dynamics 1995-2005
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432],IBM$StockPrice[301:432], lty=2,lwd=2, col="blue")
lines(GE$Date[301:432],GE$StockPrice[301:432], lty=3,lwd=2, col="green")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432], lty=4,lwd=2, col="purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432], lty=5,lwd=2, col="orange")
abline(v=as.Date(c("2000-03-01")), lwd=2)
#[1] GE

#Problem 3.2 - Visualizing Stock Dynamics 1995-2005
abline(v=as.Date(c("1995-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-01")), lwd=2)
#[1] IBM

#Problem 3.3 - Visualizing Stock Dynamics 1995-2005
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432],IBM$StockPrice[301:432], lty=2,lwd=2, col="blue")
lines(GE$Date[301:432],GE$StockPrice[301:432], lty=3,lwd=2, col="green")
lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432], lty=4,lwd=2, col="purple")
lines(Boeing$Date[301:432],Boeing$StockPrice[301:432], lty=5,lwd=2, col="orange")
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-12-01")), lwd=2)
#Procter and Gamble and Boeing

#Problem 3.4 - Visualizing Stock Dynamics 1995-2005
abline(v=as.Date(c("2004-01-01")), lwd=2)
#Boeing

#Problem 4.1 - Monthly Trends
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean) > mean(IBM$StockPrice)
# Jan, Feb, March, April, MAy

#Problem 4.2 Monthlu Trends
tapply(GE$StockPrice, months(GE$Date), mean)[which.max(tapply(GE$StockPrice, months(GE$Date), mean))]
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)

#Problem 4.3 Monthlu Trends
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(IBM$StockPrice, months(IBM$Date), mean) > mean(IBM$StockPrice)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
#Decemeber
######################################################################

########### DEMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES ########
#####################################################################

#Problem 1.1 - Loading and Summarizing the Dataset
CPS = read.csv("CPSData.csv")
summary(CPS)
str(CPS)
#131302

#Problem 1.2 - Loading and Summarizing the Dataset
table(CPS$Industry)
table(CPS$Industry)[which.max(table(CPS$Industry))]
#Educational and health services

#Problem 1.3 - Loading and Summarizing the Dataset
sort(table(CPS$State))
#New Mexico and Carolina

#Problem 1.4 - Loading and Summarizing the Dataset
1-(table(CPS$Citizenship)[3]/nrow(CPS))

#Problem 1.5 - Loading and Summarizing the Dataset
table(CPS$Race,CPS$Hispanic)
#All but asian and pacific islander

#Problem 2.1 - Evaluating Missing Values
summary(CPS)

#Problem 2.2 - Evaluating Missing Values
table(CPS$Age, is.na(CPS$Married)) #Age

#Problem 2.3 - Evaluating Missing Values
table(CPS$State, is.na(CPS$MetroAreaCode)) # 2 and 3

#Problem 2.4 - Evaluating Missing Values
table(CPS$Region, is.na(CPS$MetroAreaCode))[,2]/(table(CPS$Region, is.na(CPS$MetroAreaCode))[,1]+table(CPS$Region, is.na(CPS$MetroAreaCode))[,2])
#Midwest

#Problem 2.5 - Evaluating Missing Values
tapply(is.na(CPS$MetroAreaCode),CPS$State, mean) #Wisconsin and Montana

#Problem 3.1 - Integrating Metropolitan Area Data
MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")
nrow(MetroAreaCode)
nrow(CountryMap)

#Problem 3.2 - Integrating Metropolitan Area Data
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
#MetroArea and 34238

#Problem 3.3 - Integrating Metropolitan Area Data
sort(table(CPS$MetroArea)) #Baltimore

#Problem 3.4 - Integrating Metropolitan Area Data
tail(sort(tapply(CPS$Hispanic,CPS$MetroArea, mean)),1)

#Problem 3.5 - Integrating Metropolitan Area Data
df=as.data.frame(table(CPS$Race,CPS$MetroArea)[2,]/table(CPS$MetroArea) >= .2)
df[df$`table(CPS$Race, CPS$MetroArea)[2, ]/table(CPS$MetroArea) >= 0.2`==TRUE,]
#OR
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))  #4 Areas

#Problem 3.6 - Integrating Metropolitan Area Data
head(sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE)),1)
#Iowa City, IA

#Problem 4.1 - Integrating Country of Birth Data
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
names(CPS)
summary(CPS)
#Country 176

#Problem 4.2 - Integrating Country of Birth Data
tail(sort(table(CPS$Country)),3)  #Philippines

#Problem 4.3 - Integrating Country of Birth Data
table(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country == "United States")
1668/(1668+3736)

#Problem 4.5 - Integrating Country of Birth Data
table(CPS$MetroArea, CPS$Country == "India")
which.max(table(CPS$MetroArea, CPS$Country == "India")[,2])

table(CPS$MetroArea, CPS$Country == "Brazil")[,2]
which.max(table(CPS$MetroArea, CPS$Country == "Brazil")[,2])

table(CPS$MetroArea, CPS$Country == "Somalia")[,2]
which.max(table(CPS$MetroArea, CPS$Country == "Somalia")[,2])
