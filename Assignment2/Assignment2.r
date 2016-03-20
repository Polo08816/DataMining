#Input Data
home<-setwd(Sys.getenv("HOME"))
fpath<-file.path(home,"../git/DataMining/Assignment1","imports-85.data")
automobile <- read.csv(fpath, header = FALSE, stringsAsFactors = TRUE, col.names = c("symboling", "normalizedlosses", "make", "fueltype", "aspiration", "num-of-doors", "body-style", "drive-wheels", "engine-location", "wheel-base", "length", "width", "height", "weight", "engine-type", "num-of-cylinders", "engine-size", "fuel-system", "bore", "stroke", "compression-ratio", "horsepower", "peak-rpm", "city-mpg", "highway-mpg", "price"), na.strings = "?")
#automobile <- read.csv(file = "C:/Users/Kevin Kuo/git/DataMining/Assignment1/imports-85.data", header = FALSE, stringsAsFactors = TRUE, col.names = c("symboling", "normalized-losses", "make", "fuel-type", "aspiration", "num-of-doors", "body-style", "drive-wheels", "engine-location", "wheel-base", "length", "width", "height", "curb-weight", "engine-type", "num-of-cylinders", "engine-size", "fuel-system", "bore", "stroke", "compression-ratio", "horsepower", "peak-rpm", "city-mpg", "highway-mpg", "price"))

#show the first 10 records
#automobile[1:10,]

# Summarize the insurance risk variable
sum.symboling <- summary(automobile$symboling)
sum.symboling

# Histogram of insurance risk variable
#Set up the plot area
par(mfrow = c(1,1))
#Create histogram bars
hist(automobile$symboling,
     breaks = 20,
     xlim= c(-3,4),
     col = "blue",
     border = "black",
     ylim = c(0,100),
     xlab = "Symboling",
     ylab = "Count",
     main = "Histogram of Symboling")
#Make a box around the plot
box(which = "plot",
  lty = "solid",
  col = "black")

# Summarize the normalized losses variable
sum.normalizedlosses <- summary(automobile$normalizedlosses)
sum.normalizedlosses

# Histogram of normalized losses
#Set up the plot area
par(mfrow = c(1,1))
#Create histogram bars
hist(automobile$normalizedlosses,
     breaks = 50,
     xlim= c(25,300),
     col = "blue",
     border = "black",
     ylim = c(0,40),
     xlab = "Normalized Losses",
     ylab = "Count",
     main = "Histogram of Normalized Losses")
#Make a box around the plot
box(which = "plot",
    lty = "solid",
    col = "black")

# Summarize the horsepower variable
sum.horsepower <- summary(automobile$horsepower, na.rm = TRUE)
sum.horsepower

# Histogram of horsepower
#Set up the plot area
par(mfrow = c(1,1))
#Create histogram bars
hist(automobile$horsepower,
     breaks = 50,
     xlim= c(25,300),
     col = "blue",
     border = "black",
     ylim = c(0,50),
     xlab = "Horsepower",
     ylab = "Count",
     main = "Histogram of Horsepower")
#Make a box around the plot
box(which = "plot",
    lty = "solid",
    col = "black")

# Summarize the weight variable
sum.weight <- summary(automobile$weight, na.rm = TRUE)
sum.weight

# Histogram of weight
#Set up the plot area
par(mfrow = c(1,1))
#Create histogram bars
hist(automobile$weight,
     breaks = 50,
     xlim= c(1400,4500),
     col = "blue",
     border = "black",
     ylim = c(0,20),
     xlab = "weight",
     ylab = "Count",
     main = "Histogram of Weight")
#Make a box around the plot
box(which = "plot",
    lty = "solid",
    col = "black")


plot(automobile$horsepower, automobile$weight,
      xlim = c(0,300),
      ylim = c(1400,5000),
      xlab = "Horsepower",
      ylab = "Weight",
      main = "Scatterplot of Weight vs. Horsepower",
      type = "p",
      pch = 16,
      col = "blue")
#add open black circles
points(automobile$horsepower,
       automobile$weight,
       type = "p",
       col = "black")


#Calculate horsepower/weight ratio
hpvweight.main <- automobile$weight/automobile$horsepower
hpvweight

#Summary of weight/horsepower
sum.hpvweight <- summary(hpvweight.main)
sum.hpvweight

#Scatterplot of Symboling vs. HP/Weight
plot(automobile$symboling, hpvweight.main,
     xlim = c(-3,4),
     ylim = c(0,50),
     xlab = "Symboling",
     ylab = "Weight/HP",
     main = "Scatterplot of Weight/HP vs. Symboling",
     type = "p",
     pch = 16,
     col = "blue")
#add open black circles
points(automobile$symboling,
       hpvweight.main,
       type = "p",
       col = "black")

#Scatterplot of Normalized Losses vs. HP/Weight
plot(hpvweight.main, automobile$normalizedlosses, 
     xlim = c(10,50),
     ylim = c(50,300),
     xlab = "Weight/HP",
     ylab = "Normalized Losses",
     main = "Scatterplot of Weight/HP vs. Normalized Losses",
     type = "p",
     pch = 16,
     col = "blue")
#add open black circles
points(hpvweight.main, 
       automobile$normalizedlosses,
       type = "p",
       col = "black")


#Data Pre-processing

#Normalization
nona_horsepower<-automobile$horsepower[!is.na(automobile$horsepower)]

mmnorm.horsepower<-(nona_horsepower - min(nona_horsepower))/(max(nona_horsepower) - min(nona_horsepower))
mmnorm.horsepower

mmnorm.summary <- summary(mmnorm.horsepower)
mmnorm.summary

#Z Score
zscore.horsepower <-(mmnorm.horsepower-mean(mmnorm.horsepower))/sd(mmnorm.horsepower)
zscore.horsepower

zscore.summary <- summary(zscore.horsepower)
zscore.summary

#Decimal scaling

digits_horsepower<-nchar(floor(max(abs(automobile$horsepower))))

decscale.horsepower <-(nona_horsepower/(10^digits_horsepower))
decscale.horsepower

decsale.summary <- summary(decscale.horsepower)
decsale.summary


#Binning
#Get the sample size of the variable
n <- length(nona_horsepower)
#Decalure number of bins and bin indicator
nbins <- 3
whichbin <- c(rep(0,n))
#Equal frequency
freq <- n/nbins
#Sort data
xsorted <- sort(nona_horsepower)
xsorted
for(i in 1:nbins){
  for(j in 1:n){
  if((i-1)*freq < j && j <=i*freq)
      whichbin[j] <- i
  }
}
whichbin
summary(whichbin)

# Histogram of bin
#Set up the plot area
par(mfrow = c(1,1))
#Create histogram bars
hist(whichbin,
     breaks = 5,
     xlim= c(0,4),
     col = "blue",
     border = "black",
     ylim = c(0,100),
     xlab = "Bin",
     ylab = "Count",
     main = "Histogram of Bins")
#Make a box around the plot
box(which = "plot",
    lty = "solid",
    col = "black")

#Get the sample size of the variable
n <- length(nona_horsepower)
#Decalure number of bins and bin indicator
nbins <- 3
whichbin <- c(rep(0,n))
#Equal frequency
freq <- n/nbins
#Sort data
range_nona_horsepower <- max(nona_horsepower) - min(nona_horsepower) + 1
binwidth <- range_nona_horsepower/nbins
for(i in 1:nbins){
  for(j in 1:n){
    if((i-1)*binwidth < nona_horsepower[j] && nona_horsepower[j] <= (i)*binwidth)
        whichbin[j] <- i
  }
}
whichbin
summary(whichbin)

# Histogram of bin
#Set up the plot area
par(mfrow = c(1,1))
#Create histogram bars
hist(whichbin,
     breaks = 5,
     xlim= c(0,4),
     col = "blue",
     border = "black",
     ylim = c(0,150),
     xlab = "Bin",
     ylab = "Count",
     main = "Histogram of Bins")
#Make a box around the plot
box(which = "plot",
    lty = "solid",
    col = "black")


#Transformations

nona_normalizedlosses <- automobile$normalizedlosses[!is.na(automobile$normalizedlosses) | !is.na(automobile$horsepower | !is.na(automobile$weight))]
nona_normalizedlosses
summary(nona_normalizedlosses)

nona_horsepower2 <- automobile$horsepower[!is.na(automobile$normalizedlosses) | !is.na(automobile$horsepower) | !is.na(automobile$weight)]
nona_horsepower2
summary(nona_horsepower2)

nona_weight2 <- automobile$weight[!is.na(automobile$normalizedlosses) | !is.na(automobile$horsepower) | !is.na(automobile$weight)]
nona_weight2
summary(nona_weight2)

hpvweight2 <- nona_weight2/nona_horsepower2
hpvweight2
summary(hpvweight2)

#Natural Log
natlog_hpvweight2 <- log(hpvweight2)
natlog_hpvweight2
summary(natlog_horsepower)


#Scatterplot of Normalized Losses vs. NaturalLog of Horsepower
plot(natlog_horsepower, nona_normalizedlosses, 
     xlim = c(0,6),
     ylim = c(0,300),
     xlab = "Natural Log of Weight/HP",
     ylab = "Normalized Losses",
     main = "Scatterplot of Natural Log of Weight/HP vs. Normalized Losses",
     type = "p",
     pch = 16,
     col = "blue")
#add open black circles
points(natlog_horsepower, 
       nona_normalizedlosses,
       type = "p",
       col = "black")

# #Square Root transformation
sq_rt_hpvweight2 <- sqrt(hpvweight2)
sq_rt_hpvweight2
summary(sq_rt_hpvweight2)

#Scatterplot of Normalized Losses vs. Square Root of Weight/HP
plot(sq_rt_hpvweight2, nona_normalizedlosses, 
     xlim = c(3,8),
     ylim = c(0,300),
     xlab = "Square Root of Weight/HP",
     ylab = "Normalized Losses",
     main = "Scatterplot of Square Root of Weight/HP vs. Normalized Losses",
     type = "p",
     pch = 16,
     col = "blue")
#add open black circles
points(sq_rt_hpvweight2, 
       nona_normalizedlosses,
       type = "p",
       col = "black")

# #Inverse Square Root transformation
invsqrt_hpvweight2 <- 1/sqrt(hpvweight2)
invsqrt_hpvweight2
summary(invsqrt_hpvweight2)

#Scatterplot of Normalized Losses vs. Inverse Square Root of Weight/HP
plot(invsqrt_hpvweight2, nona_normalizedlosses, 
     xlim = c(0,.5),
     ylim = c(0,300),
     xlab = "Inverse Square Root of Weight/HP",
     ylab = "Normalized Losses",
     main = "Scatterplot of Square Root of Weight/HP vs. Normalized Losses",
     type = "p",
     pch = 16,
     col = "blue")
#add open black circles
points(invsqrt_hpvweight2, 
       nona_normalizedlosses,
       type = "p",
       col = "black")


#Regression model
fit <-lm(hpvweight.main ~ automobile$normalizedlosses)
summary(fit)

#Correlation models
corr <-cbind(hpvweight.main,
             automobile$normalizedlosses)
corr_test <-cor.test(hpvweight.main,automobile$normalizedlosses)
corr_test$p.value