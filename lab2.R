# Input data set Churn into Data Frame "Churn"
churn <- read.csv(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Lab Materials/Data/churn.txt", stringsAsFactors = TRUE)
churn[1:10,]

# Summarize the Churn variable
sum.churn <- summary(churn$Churn)
sum.churn

# Calculate proportion of churners
prop.churn <- sum(churn$Churn == "True")/length(churn$Churn)
prop.churn

# Bar chart of variable Churn
barplot(sum.churn,
        ylim = c(0,3000),
        main = "Bar Graph of Churners and Non-Churners",
        col="lightblue")
box(which = "plot",
    lty = "solid",
    col = "black")

# Make a table for counts for Churn and International Plan
counts <- table(churn$Churn, churn$Int.l.Plan, dnn=c("Churn","International Plan"))
counts

# Create a table with sums for both variables
sumtable <- addmargins(counts, FUN = sum)
sumtable

# Overlayed bar chart
barplot(counts,
        legend = rownames(counts),
        col = c("blue","red"),
        ylim = c(0,3300),
        ylab = "Count",
        xlab = "International Plan",
        main = "Comparison Bar Chart:
        Churn Proportions by International Plan")
box(which = "plot",
    lty = "solid",
    col="black")

# Create a table of proportions over rows
row.margin <- round(prop.table(counts,
                               margin = 1),
                    4)*100
row.margin

# Create a table of proportions over columns
col.margin <- round(prop.table(counts,
                               margin = 2),
                    4)*100
col.margin

# Histogram of non-overlayed Customer Service Calls
hist(churn$CustServ.Calls,
     xlim = c(0,10),
     col = "lightblue",
     ylab = "Count",
     xlab = "Customer Service Calls",
     main = "Histogram of Customer Service Calls")

# Clustered Bar Chart, with legend
barplot(counts,
        col = c("blue", "red"),
        ylim = c(0,3300),
        ylab = "Count",
        xlab = "International Plan",
        main = "Churn Count by International Plan",
        beside = TRUE)
legend("topright",
       c(rownames(count)),
       col = c("blue", "red"),
       pch = 15,
       title = "Churn")
box(which = "plot",
    lty = "solid",
    col = "black")

# Download and install the R Package ggplot2
install.packages("ggplot2")
  # Pick any CRAN number
  # (see example image)
  # Open the new package
library(ggplot2)

# Clustered Bar Chart of Churn and Int'l Plan with Legend
barplot(t(counts),
        col = c("blue", "green"),
        ylim = c(0,3300),
        ylab = "Counts",
        xlab = "Churn",
        main = "International Plan Count by Churn",
        beside = TRUE)
legend("topright",
       c(rownames(counts)),
       col = c("blue","green"),
       pch=15,
       title = "Int'l Plan")
box(which = "plot",
    lty = "solid",
    col = "black")

# Overlayed bar charts

# Two-sample T-Test for Int'l Calls
  #Partition data
churn.false <- subset(churn,
                      churn$Churn == "False")
churn.true <- subset(churn,
                     churn$Churn == "True")
  #Run the test
t.test(churn.false$Intl.Calls,
       churn.true$Intl.Calls)

# Scatterplot matrix
pairs(~churn$Day.Mins+
        churn$Day.Calls+
        churn$Day.Charge)

# Regression of Day Charge vs Day Minutes
fit <- lm(churn$Day.Charge ~
            churn$Day.Mins)
summary(fit)

# Scatterplot of Evening Minutes and Day Minutes, colored by Churn
plot(churn$Eve.Mins,
     churn$Day.Mins,
     xlim = c(0,400),
     ylim = c(0,400),
     xlab = "Evening Minutes",
     ylab = "Day Minutes",
     main = "Scatterplot of Day and Evening Minutes by Churn",
     col = ifelse(churn$Churn == "True",
                  "red",
                  "blue"))
legend("topright",
       c("True",
         "False"),
       col = c("red",
               "blue"),
       pch = 1,
       title = "Churn")

