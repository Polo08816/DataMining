#Input Data
home<-setwd(Sys.getenv("HOME"))
fpath<-file.path(home,"../git/DataMining/Assignment3","haberman.data")
habermansurvival <- read.csv(fpath, header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("age", "operation_year", "positive_axillary_nodes", "survival_status"), na.strings = "?")
# habermansurvival[1:30,]
m = cbind(habermansurvival$operation_year, habermansurvival$positive_axillary_nodes)


#load libraries
library(cluster)
library(fpc)


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

