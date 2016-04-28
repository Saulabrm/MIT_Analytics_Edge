setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week7")

#####Intro WHO DATASET ###################
WHO = read.csv("WHO.csv")
str(WHO)

plot(WHO$GNI,WHO$FertilityRate)
install.packages("ggplot2")
library(ggplot2)

scatterplot = ggplot(WHO, aes(x= GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()

scatterplot + geom_point(color ="blue", size = 3, shape = 17)
scatterplot + geom_point(color ="darkred", size = 3, shape = 8)

fertilityGNIplot = scatterplot + geom_point(color ="darkred", size = 3, shape = 8) + ggtitle("Fertility Rate vs. Gross National Income")
pdf("Myplot.pdf")
print(fertilityGNIplot)
dev.off()

#Colored by region
ggplot(WHO, aes(x=GNI, y=FertilityRate, color=Region)) + geom_point()

#Colored by LifeExpectancy
ggplot(WHO, aes(x=GNI, y=FertilityRate, color=LifeExpectancy)) + geom_point()

ggplot(WHO, aes(x=FertilityRate, y=Under15)) + geom_point()

#Linear Model
model = lm(Under15 ~ log(FertilityRate), data=WHO)
summary(model)

#Print the linear model   
ggplot(WHO, aes(x=FertilityRate, y=Under15)) + geom_point() + stat_smooth(method="lm", level = 0.99, se=FALSE, color = "orange")    

#QQ
ggplot(WHO, aes(x = FertilityRate, y = Under15, color=Region)) + geom_point() + scale_color_brewer(palette="Dark2")
#####Intro WHO DATASET ###################

mvt = read.csv("mvt.csv", stringsAsFactors = F)
str(mvt)

#Format Date
mvt$Date = strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour
str(mvt)

#Line plots
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts)

ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

#Lets order the days
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered = TRUE,
  levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week")
  +ylab("Total Motor Vehicle Thefts")

#QQ
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2) + xlab("Day of the Week")
+ylab("Total Motor Vehicle Thefts")

#QQ2
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=.3) + xlab("Day of the Week")
+ylab("Total Motor Vehicle Thefts")

#Prepare to Heatmap
DayHourCounts = as.data.frame(table(mvt$Weekday,mvt$Hour))
str(DayHourCounts)

DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

#Line plot
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1, size=1.5))

DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered = TRUE,
                                  levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

#Heatmap
ggplot(DayHourCounts, aes(x=Hour, y=Var1)) +  geom_tile(aes(fill=Freq)) +
    scale_fill_gradient(name="Total MV Thefts", low="white", high="black") +
    theme(axis.title.y= element_blank())

#Plot Crime in Map of Chicago
install.packages("maps")
install.packages("ggmap")

library(maps)
library(ggmap)

chicago = get_map(location = "chicago", zoom = 11)
ggmap(chicago)

ggmap(chicago) + geom_point(data=mvt[1:100,], aes(x=Longitude, y=Latitude))
LatLonCounts= as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
str(LatLonCounts)
LatLonCounts$Long=as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat=as.numeric(as.character(LatLonCounts$Var2))

ggmap(chicago) + geom_point(data=LatLonCounts, aes(x=Long, y=Lat, color = Freq, size=Freq)) + scale_color_gradient(low="yellow",high = "red")

#Heatmap
ggmap(chicago) + geom_tile(data=LatLonCounts, aes(x=Long, y=Lat, alpha=Freq), fill="red")

#Redo without the values 0.
LatLonCounts2= subset(LatLonCounts,Freq>0)
ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long, y=Lat, alpha=Freq), fill="red")

##### MURDERS Dataset##########
murders = read.csv("murders.csv")
str(murders)
statesMap = map_data("state")
str(statesMap)

#Plot the map
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white" , color="black")

#Match States names
murders$region = tolower(murders$State)

#Merge dataframes
MurderMap = merge(statesMap,murders, by="region")

#Plot by Murders
ggplot(MurderMap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(color="black") + scale_fill_gradient(low="white",high="red", guide="legend")

#Plot by Population
ggplot(MurderMap, aes(x=long, y=lat, group=group, fill=Population)) + geom_polygon(color="black") + scale_fill_gradient(low="white",high="red", guide="legend")


MurderMap$MurderRate =  MurderMap$Murders/MurderMap$Population*100000

#Plot by Murder Rate
ggplot(MurderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low="white",high="red", guide="legend")

ggplot(MurderMap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") + scale_fill_gradient(low="white",high="red", guide="legend", limits = c(0,10))

#Pplot by gun ownership
ggplot(MurderMap, aes(x=long, y=lat, group=group, fill=GunOwnership)) + geom_polygon(color="black") + scale_fill_gradient(low="white",high="red", guide="legend")
#Montana
