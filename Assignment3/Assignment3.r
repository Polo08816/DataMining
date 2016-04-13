#Input Data
home<-setwd(Sys.getenv("HOME"))
fpath<-file.path(home,"../git/DataMining/Assignment3","haberman.data")
habermansurvivalraw <- read.csv(fpath, header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("age", "operation_year", "positive_axillary_nodes", "survival_status"), na.strings = "?")
# habermansurvival[1:30,]

# remove NA values
habermansurvival = na.omit(habermansurvivalraw)

m = cbind(habermansurvival$age, habermansurvival$operation_year)


#load libraries
library(cluster)
library(fpc)


# Summarize Class Output
sum.class <- summary(habermansurvival$survival_status)
sum.class


# Summarize age
sum.age <- summary(habermansurvival$age)
sum.age


# Summarize operation year
sum.operation_year <- summary(habermansurvival$operation_year)
sum.operation_year


# Summarize axillary nodes
sum.positive_axillary_nodes <- summary(habermansurvival$positive_axillary_nodes)
sum.positive_axillary_nodes


###############################################################################
#k-means
###############################################################################

#plot the toy dataset
plot(m, main = "Results Unclustered", xlab = "Age", ylab = "Operation Year")
#run k-means with k = 2
km2 <- kmeans(m, centers = 2) 
km2

#visualize results colored by cluster
plot(m, col=km2$cluster, main = "Results Colored By 2 Clusters", xlab = "Age", ylab = "Operation Year")

#plot cluster centers
points(km2$centers,pch='x',cex=1.5)

#run k-means with k = 3
km3 <- kmeans(m, centers = 3) 
km3

#visualize results colored by cluster
plot(m, col=km3$cluster, main = "Results Colored By 3 Clusters", xlab = "Age", ylab = "Operation Year")

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
plot(m, col=pam2$cluster, main = "Results Colored By 2 Cluster Centers", xlab = "Age", ylab = "Operation Year")

#plot cluster centers
points(pam2$medoids,pch='O',cex=1.5)

#run PAM with k = 3
pam3 <- pam(m, 3)

#visualize results colored by cluster
plot(m, col=pam3$cluster, main = "Results Colored By 3 Cluster Centers", xlab = "Age", ylab = "Operation Year")

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




plot(m, main = "Results Unclustered", xlab = "Age", ylab = "Operation Year")
#Run dbscan with eps = 5 and MinPts = 5
dbr <- dbscan(m, eps=5, MinPts=5)
str(dbr)
plot(m, col=dbr$cluster+1L)

#Run dbscan with eps = 7 and MinPts = 5
dbr <- dbscan(m, eps=7, MinPts=5)
str(dbr)
plot(m, col=dbr$cluster+1L, main = "Results EPS = 7", xlab = "Age", ylab = "Operation Year")

#Run dbscan with eps = 10 and MinPts = 5
dbr <- dbscan(m, eps=10, MinPts=5)
str(dbr)
plot(m, col=dbr$cluster+1L, main = "Results EPS = 10", xlab = "Age", ylab = "Operation Year")

#Run dbscan with eps = 20 and MinPts = 5 
dbr <- dbscan(m, eps=20, MinPts=5)
str(dbr)
plot(m, col=dbr$cluster+1L)

#Run dbscan with eps = 50 and MinPts = 5 
dbr <- dbscan(m, eps=50, MinPts=5)
str(dbr)
plot(m, col=dbr$cluster+1L)

# Silhouette plot
d <- dist(m)
sil <- silhouette(dbr$cluster,d)
plot(sil)