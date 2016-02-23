#Input Data
automobile <- read.csv(file = "C:/Users/J14688/git/DataMining/Assignment1/imports-85.data", header = FALSE, stringsAsFactors = TRUE, col.names = c("symboling", "normalized-losses", "make", "fuel-type", "aspiration", "num-of-doors", "body-style", "drive-wheels", "engine-location", "wheel-base", "length", "width", "height", "curb-weight", "engine-type", "num-of-cylinders", "engine-size", "fuel-system", "bore", "stroke", "compression-ratio", "horsepower", "peak-rpm", "city-mpg", "highway-mpg", "price"))

#show the first 10 records
automobile[1:10,]

sum.automobile <- summary(automobile$std)