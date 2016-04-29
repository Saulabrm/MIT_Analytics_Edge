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


##########     RECITATION      ###########
######### GOOD PLOT, BAD PLOT ############

library(ggplot2)
intl = read.csv("intl.csv")
str(intl)

#Barplot
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = PercentOfIntl))

#Order Region factor (decreasing order)
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)

#Convert decimals into perentages
intl$PercentOfIntl = intl$PercentOfIntl*100

ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat = "identity", fill = "dark blue") +
  geom_text(aes(label = PercentOfIntl), vjust= -0.4) +  #Vjust moves the labels down, or up
  ylab("Percent of International Students") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(angle=45, hjust =1))

#World Map
library(ggmap)
intlall= read.csv("intlall.csv", stringsAsFactors = FALSE)
head(intlall)
#NAs to 0
intlall[is.na(intlall)] = 0
head(intlall)

world_map = map_data("world")
str(world_map)

world_map <-  merge(world_map, intlall, by.x="region" , by.y="Citizenship")
str(world_map)

# Plot the map, the merge reordered the data..
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") + 
  coord_map("mercator")

# Reorder the data
world_map = world_map[order(world_map$group, world_map$order),]

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") + 
  coord_map("mercator")

table(intlall$Citizenship) #China has a differnet name
intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"

#Repeat Merge
world_map = map_data("world")
world_map <-  merge(world_map, intlall, by.x="region" , by.y="Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]

#Map
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") + 
  coord_map("mercator")

#3D Map
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") + 
  coord_map("ortho", orientation = c(20,30,0))

#### HouseHolds

households = read.csv("households.csv")
str(households)

library(reshape2)
households[,1:2]

head(melt(households, id="Year"))
households[,1:3]

melt(households, id="Year")

#Plot
ggplot(melt(households, id="Year"), aes(x=Year, y=value, color=variable)) +
  geom_line(size=2) + geom_point(size=5) + ylab("Percentage of Households")
