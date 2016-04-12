#install packages (if needed)
install.packages("cluster")
install.packages("fpc")
#load libraries
library(cluster)
library(fpc)
###############################################################################
#k-means example
###############################################################################
#create 2D toy dataset
a <- c(2,0)
b <- c(1,2)
c <- c(2,2)
d <- c(3,2)
e <- c(2,3)
f <- c(6,8)
g <- c(6,7)
h <- c(6,6)
i <- c(7,8)
j <- c(8,7)
k <- c(1,6)
l <- c(1,8)
m <- c(2,6)
n <- c(2,8)
o <- c(1,9)

m = rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

#plot the toy dataset
plot(m)
#run k-means with k = 2
km2 <- kmeans(m, centers = 2) 
km2

#visualize results colored by cluster
plot(m, col=km2$cluster)

#plot cluster centers
points(km2$centers,pch='x',cex=1.5)

#run k-means with k = 3
km3 <- kmeans(m, centers = 3) 
km3

#visualize results colored by cluster
plot(m, col=km3$cluster)

#plot cluster centers
points(km3$centers,pch='x',cex=1.5)

###############################################################################
#evaluating k-means in R
###############################################################################
#create distance matrix for cluster.stats
distm <- dist(m)  
#calculate cluster statistics for km2
cstatskm2 = cluster.stats(distm,km2$cluster)
#calculate cluster statistics for km3
cstatskm3 = cluster.stats(distm,km3$cluster) 
#evaluate between and within cluster distances
cstatskm2$average.between
cstatskm2$average.within
cstatskm3$average.between
cstatskm3$average.within


#kmedoids example
distm <- dist(m) 
#run PAM with k = 2
pam2 <- pam(m, 2)

#visualize results colored by cluster
plot(m, col=pam2$cluster)

#plot cluster centers
points(pam2$medoids,pch='O',cex=1.5)

#run PAM with k = 3
pam3 <- pam(m, 3)

#visualize results colored by cluster
plot(m, col=pam3$cluster)

#plot cluster centers
points(pam3$medoids,pch='O',cex=1.5)

#evaluate PAM
cstatspam2 <- cluster.stats(distm,pam2$cluster)
cstatspam3 <- cluster.stats(distm,pam3$cluster)


#evaluate between and within cluster distances
cstatskm2$average.between
cstatskm2$average.within
cstatskm3$average.between
cstatskm3$average.within

#single linkage example
agn <- agnes(m, diss=FALSE, stand=FALSE, method="single") 
dend_agn <- as.dendrogram(agn)
plot(dend_agn, xlab="Index of Data Points", ylab = "Steps", main = "Single-Linkage Clustering")

#complete linkage example
agn <- agnes(m, diss=FALSE, stand=FALSE, method="complete") 
dend_agn <- as.dendrogram(agn)
plot(dend_agn, xlab="Index of Data Points", ylab = "Steps", main = "Complete-Linkage Clustering")

