#Input Data
home<-setwd(Sys.getenv("HOME"))
fpath<-file.path(home,"../git/DataMining/Assignment4","OnlineRetail.csv")
# onlineretailraw <- read.transactions(fpath, header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("InvoiceNo", "StockCode", "Description", "Quantity", "InvoiceDate", "UnitPrice", "CustomerID", "Country"), na.strings = "?")
# onlineretailraw <- read.transactions(file = "C:/Users/Kevin Kuo/git/DataMining/Assignment4/OnlineRetail.csv", format = "basket", sep = ",", skip = 1, quote = "", rm.duplicates = TRUE)
onlineretailraw <- read.transactions(file = "C:/Users/Kevin Kuo/git/DataMining/Assignment4/house-votes-84.data", format = "basket", sep = ",", skip = 0, quote = "", rm.duplicates = TRUE)

library(arules)
library(datasets)
library(Matrix)

summary(onlineretailraw)

itemFrequencyPlot(onlineretailraw, support = 0.5, cex.names = 1.0)

rules <- apriori(onlineretailraw, parameter = list(support = 0.01, confidence = 0.6))