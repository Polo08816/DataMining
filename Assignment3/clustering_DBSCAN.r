#dbscan example
library(cluster)
library(fpc)
data(ruspini)
plot(ruspini)
#Run dbscan with eps = 5 and MinPts = 5
dbr <- dbscan(ruspini, eps=5, MinPts=5)
str(dbr)
plot(ruspini, col=dbr$cluster+1L)

#Run dbscan with eps = 7 and MinPts = 5
dbr <- dbscan(ruspini, eps=7, MinPts=5)
str(dbr)
plot(ruspini, col=dbr$cluster+1L)

#Run dbscan with eps = 10 and MinPts = 5
dbr <- dbscan(ruspini, eps=10, MinPts=5)
str(dbr)
plot(ruspini, col=dbr$cluster+1L)

#Run dbscan with eps = 20 and MinPts = 5 
dbr <- dbscan(ruspini, eps=20, MinPts=5)
str(dbr)
plot(ruspini, col=dbr$cluster+1L)

#Run dbscan with eps = 50 and MinPts = 5 
dbr <- dbscan(ruspini, eps=50, MinPts=5)
str(dbr)
plot(ruspini, col=dbr$cluster+1L)

# Silhouette plot
d <- dist(ruspini)
sil <- silhouette(dbr$cluster,d)
plot(sil)


