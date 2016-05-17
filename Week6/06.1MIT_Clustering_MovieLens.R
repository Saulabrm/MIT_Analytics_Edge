#Unit 6 Clustering _ MovieLens
setwd("/Users/saulgarcia/Desktop/Github/MIT_Analytics_Edge/Week5")

# url <- "http://files.grouplens.org/datasets/movielens/ml-100k/u.item"

#Read Dataset

movies = read.table("movielens.txt", header=FALSE, sep="|", quote="\"")
str(movies)
colnames(movies) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure",
                     "Animation","Childrens","Comedy","Crime", "Documentary","Drama","Fantasy","FilmNoir",
                     "Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")

#Remove variable
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
str(movies)

#QuickQuestion
table(movies$Comedy)
table(movies$Western)
table(movies$Romance & movies$Drama)
table(movies$Drama)

#clusterMovies = hclust(distances, method = "ward.D")

distances = dist(movies[2:20], method="euclidean")
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies)
clusterGroups = cutree(clusterMovies, k=10)

#Compute the number of movies in each cluster
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

subset(movies,Title=="Men in Black (1997)") #This is in row 257
clusterGroups[257]

cluster5= subset(movies, clusterGroups==5)
cluster5$Title[1:10]

spl = split(movies[2:20],clusterGroups)
lapply(spl, colMeans) #This is the list she inputs to excel

##########
##QuickQuestion
clusterG = cutree(clusterMovies, k=2)
lapply(spl2,colMeans)