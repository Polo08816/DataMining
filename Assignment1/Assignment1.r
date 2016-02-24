#Input Data
automobile <- read.csv(file = "C:/Users/J14688/git/DataMining/Assignment1/imports-85.data", header = FALSE, stringsAsFactors = TRUE, col.names = c("symboling", "normalized-losses", "make", "fuel-type", "aspiration", "num-of-doors", "body-style", "drive-wheels", "engine-location", "wheel-base", "length", "width", "height", "curb-weight", "engine-type", "num-of-cylinders", "engine-size", "fuel-system", "bore", "stroke", "compression-ratio", "horsepower", "peak-rpm", "city-mpg", "highway-mpg", "price"))

#show the first 10 records
automobile[1:10,]

# Summarize the insurance risk variable
sum.automobile <- summary(automobile$symboling)
sum.automobile

# Calculate proportion of symboling above 0
prop.automobile <- sum(automobile$symboling > "0", na.rm = TRUE)/length(automobile$symboling)
prop.automobile

# Bar chart of variable symboling
barplot(automobile$symboling,
        ylim = c(-4,4),
        main = "Bar Graph of Symboling",
        col="lightblue")
box(which = "plot",
    lty = "solid",
    col = "black")

# Make a table for counts for symboling and normalized losses
counts <- table(automobile$symboling, automobile$normalized.losses, dnn=c("symboling","normalized-losses"))
counts

# Create a table with sums for both variables
sumtable <- addmargins(counts, FUN = sum)
sumtable


# Overlayed bar chart
barplot(counts,
        legend = rownames(counts),
        col = c("blue","red"),
        ylim = c(0,15),
        ylab = "Count",
        xlab = "symboling",
        main = "Comparison Bar Chart:
        Symboling proportions")
box(which = "plot",
    lty = "solid",
    col="black")