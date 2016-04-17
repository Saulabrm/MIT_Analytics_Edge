#Unit 6.2 Clustering _ Recitation
setwd("/Users/saulgarcia/Desktop/Github/MOOCs/MIT Analyticals Edge/Week6")

flower = read.csv("flower.csv", header=FALSE)
str(flower)

flowerMatrix = as.matrix(flower)
str(flowerMatrix)
flowerVector  = as.vector(flowerMatrix)
str(flowerVector)
flowerVector2 = as.vector(flower)
str(flowerVector2)

#Clustering
distance = dist(flowerVector, method ="euclidean")
clusterIntensity = hclust(distance, method = "ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k=3 , border="red")
#Split data into these 3 clusters
flowerCluster = cutree(clusterIntensity, k=3)
#check the mean by each cluster
tapply(flowerVector,flowerCluster, mean)
#See the segmentation of image
dim(flowerCluster) = c(50,50)
image(flowerCluster, axes=F)
#Original Image
image(flowerMatrix,axes=F, col = grey(seq(0,1,length=256)))

######     MRI IMAGE    ########
healthy = read.csv("healthy.csv", header = FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes = FALSE, col = grey(seq(0,1,length=256)))
healthyVector= as.vector(healthyMatrix)
distance = dist(healthyVector, method="euclidean") #The distance is hard to compute.
str(healthyVector)
n = 365636
n*(n-1)/2  #how big the dataframe wil be

#Lets use K Means.

k=5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
healthyClusters = KMC$cluster
KMC$centers[2] #Substitutes tapply for the previous example
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes=FALSE, col= rainbow(k)) #Can we use this clusters to identify tumors?

#Read tumor.csv
tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)
library(flexclust)
#healthyVector will be our training set
KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))
