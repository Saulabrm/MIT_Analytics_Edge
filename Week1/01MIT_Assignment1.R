library(dplyr)

#Assignment 1
getwd()
list.files()
poll = read.csv("AnonymityPoll.csv")
str(poll)
summary(poll)

#Q1 How many people participated in the poll?
#Q1A: data.frame':	1002 obs. of  13 variables:

summarypoll$Smartphone)
table(poll$Smartphone)

#Q2.1  How many interviewees responded that they use a smartphone?
# 487

#Q2.1How many interviewees responded that they don't use a smartphone?
# 472

#Q2.1How many interviewees did not respond to the question, resulting in a missing value, or NA, in the summary() output?
# 43

table(poll$Sex, poll$Region)

# Which of the following are states in the Midwest census region? (Select all that apply.)
census<-table(poll$State, poll$Region)
censusdf<- as.data.frame(census)
names(censusdf) = c("State", "Region", "Frequency")
censusdf %>% filter(Region == "Midwest" & Frequency >0) %>% select(State)
# State
# 1      Illinois
# 2       Indiana
# 3          Iowa
# 4        Kansas
# 5      Michigan
# 6     Minnesota
# 7      Missouri
# 8      Nebraska
# 9  North Dakota
# 10         Ohio
# 11 South Dakota
# 12    Wisconsin
censusdf %>% filter(Region == "South") %>% arrange(desc(Frequency)) %>% top_n(n=1)

#Problem 2.1
table(poll$Internet.Use,poll$Smartphone)
#    0   1
# 0 186  17
# 1 285 470

# 186 is not having a internet and not having smartphone
# 17 is not having used internet and having used smartphone
# 285 is having interent and not having a smartphone
# 470 is having Internet and Smartphone

#Problem 2.2
summary(poll)
#1 and 43

#Problem 2.3
limited<- subset(poll, Internet.Use==1 | Smartphone ==1)
str(limited)
nrow(limited)

#Problem 3.1
summary(limited)
colnames(limited)[colSums(is.na(limited)) > 0]
# [1] "Smartphone"             "Age"                    "Conservativeness"       "Worry.About.Info"      
# [5] "Privacy.Importance"     "Anonymity.Possible"     "Tried.Masking.Identity" "Privacy.Laws.Effective"

#Probem3.2
mean(poll$Info.On.Internet, na.rm = T)
# [1] 3.795455

#Problem3.3
table(poll$Info.On.Internet)
# 0   1   2   3   4   5   6   7   8   9  10  11 
# 105  84  95 101 104  94  67  63  40  18  13   8 


#Problem3.4
table(limited$Worry.About.Info)[2] / sum(table(limited$Worry.About.Info))
#0.4886076

#Problem3.5
mean(limited$Anonymity.Possible, na.rm=T)
# [1] 0.3691899

#Problem3.6
mean(limited$Tried.Masking.Identity, na.rm = T)
# [1] 0.1632653
#Problem3.7
mean(limited$Privacy.Laws.Effective, na.rm=T)
#[1] 0.2558459

#Problem4.1
hist(limited$Age)
#60
#Problem4.2
plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))
# [1] 6
#Problem 4.3
jitter(c(1,2,3))
# jitter adds or subtracts a small amount of
# random noise to the values passed to it, and two runs will yield different results 

#Problem4.4
#Older age seems moderately associated with a smaller value for Info.On.Internet 

#Problem4.5
tapply(limited$Info.On.Internet,limited$Smartphone, summary)
#4.368   for Smartphones
#2.923   for non Smartphone users

#Problem4.6
tapply(limited$Tried.Masking.Identity,limited$Smartphone, summary)
#.1925 for Smartphone users
#.1174 for non Smartphone users