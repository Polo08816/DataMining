library(arules)
library(datasets)
library(Matrix)
library(xlsx)

#Input Data
votingRecordsRaw <- read.csv(file = "C:/Users/Kevin Kuo/git/DataMining/Assignment4/house-votes-84.data", header = FALSE, sep = ",", stringsAsFactors = FALSE, col.names = c("Party", "HandicappedInfants", "WaterProjectCostSharing", "BudgetResolution", "PhysicianFeeFreeze", "ElSalvadorAid", "ReligiousSchools", "SatelliteTestBan", "NicaraguanAid", "MxMissle", "Immigration", "SynFuelsCutback", "EducationSpending", "SuperFundSue", "Crime", "DutyFreeExports", "ExportSouthAfrica"), na.strings = "?")
votingRecordsProcessed <- read.csv(file = "C:/Users/Kevin Kuo/git/DataMining/Assignment4/house-votes-84.data", header = FALSE, sep = ",", stringsAsFactors = FALSE)
votingRecordsRaw.ID <- seq.int(nrow(votingRecordsRaw))

summary(votingRecordsRaw)
votingRecordsRaw[1:20,]

write.xlsx(votingRecordsRaw, "C:/Users/Kevin Kuo/git/DataMining/Assignment4/house-votes-84.xlsx")

votingRecordsProcessed$V1 <- as.factor(votingRecordsProcessed$V1)

votingRecordsProcessed$V2[votingRecordsProcessed$V2=="y"]<-"2Y"
votingRecordsProcessed$V2[votingRecordsProcessed$V2=="n"]<-"2N"
votingRecordsProcessed$V2[votingRecordsProcessed$V2=="?"]<-"2?"
votingRecordsProcessed$V2 <- as.factor(votingRecordsProcessed$V2)

votingRecordsProcessed$V3[votingRecordsProcessed$V3=="y"]<-"3Y"
votingRecordsProcessed$V3[votingRecordsProcessed$V3=="n"]<-"3N"
votingRecordsProcessed$V3[votingRecordsProcessed$V3=="?"]<-"3?"
votingRecordsProcessed$V3 <- as.factor(votingRecordsProcessed$V3)

votingRecordsProcessed$V4[votingRecordsProcessed$V4=="y"]<-"4Y"
votingRecordsProcessed$V4[votingRecordsProcessed$V4=="n"]<-"4N"
votingRecordsProcessed$V4[votingRecordsProcessed$V4=="?"]<-"4?"
votingRecordsProcessed$V4 <- as.factor(votingRecordsProcessed$V4)

votingRecordsProcessed$V5[votingRecordsProcessed$V5=="y"]<-"5Y"
votingRecordsProcessed$V5[votingRecordsProcessed$V5=="n"]<-"5N"
votingRecordsProcessed$V5[votingRecordsProcessed$V5=="?"]<-"5?"
votingRecordsProcessed$V5 <- as.factor(votingRecordsProcessed$V5)

votingRecordsProcessed$V6[votingRecordsProcessed$V6=="y"]<-"6Y"
votingRecordsProcessed$V6[votingRecordsProcessed$V6=="n"]<-"6N"
votingRecordsProcessed$V6[votingRecordsProcessed$V6=="?"]<-"6?"
votingRecordsProcessed$V6 <- as.factor(votingRecordsProcessed$V6)

votingRecordsProcessed$V7[votingRecordsProcessed$V7=="y"]<-"7Y"
votingRecordsProcessed$V7[votingRecordsProcessed$V7=="n"]<-"7N"
votingRecordsProcessed$V7[votingRecordsProcessed$V7=="?"]<-"7?"
votingRecordsProcessed$V7 <- as.factor(votingRecordsProcessed$V7)

votingRecordsProcessed$V8[votingRecordsProcessed$V8=="y"]<-"8Y"
votingRecordsProcessed$V8[votingRecordsProcessed$V8=="n"]<-"8N"
votingRecordsProcessed$V8[votingRecordsProcessed$V8=="?"]<-"8?"
votingRecordsProcessed$V8 <- as.factor(votingRecordsProcessed$V8)

votingRecordsProcessed$V9[votingRecordsProcessed$V9=="y"]<-"9Y"
votingRecordsProcessed$V9[votingRecordsProcessed$V9=="n"]<-"9N"
votingRecordsProcessed$V9[votingRecordsProcessed$V9=="?"]<-"9?"
votingRecordsProcessed$V9 <- as.factor(votingRecordsProcessed$V9)

votingRecordsProcessed$V10[votingRecordsProcessed$V10=="y"]<-"10Y"
votingRecordsProcessed$V10[votingRecordsProcessed$V10=="n"]<-"10N"
votingRecordsProcessed$V10[votingRecordsProcessed$V10=="?"]<-"10?"
votingRecordsProcessed$V10 <- as.factor(votingRecordsProcessed$V10)

votingRecordsProcessed$V11[votingRecordsProcessed$V11=="y"]<-"11Y"
votingRecordsProcessed$V11[votingRecordsProcessed$V11=="n"]<-"11N"
votingRecordsProcessed$V11[votingRecordsProcessed$V11=="?"]<-"11?"
votingRecordsProcessed$V11 <- as.factor(votingRecordsProcessed$V11)

votingRecordsProcessed$V12[votingRecordsProcessed$V12=="y"]<-"12Y"
votingRecordsProcessed$V12[votingRecordsProcessed$V12=="n"]<-"12N"
votingRecordsProcessed$V12[votingRecordsProcessed$V12=="?"]<-"12?"
votingRecordsProcessed$V12 <- as.factor(votingRecordsProcessed$V12)

votingRecordsProcessed$V13[votingRecordsProcessed$V13=="y"]<-"13Y"
votingRecordsProcessed$V13[votingRecordsProcessed$V13=="n"]<-"13N"
votingRecordsProcessed$V13[votingRecordsProcessed$V13=="?"]<-"13?"
votingRecordsProcessed$V13 <- as.factor(votingRecordsProcessed$V13)

votingRecordsProcessed$V14[votingRecordsProcessed$V14=="y"]<-"14Y"
votingRecordsProcessed$V14[votingRecordsProcessed$V14=="n"]<-"14N"
votingRecordsProcessed$V14[votingRecordsProcessed$V14=="?"]<-"14?"
votingRecordsProcessed$V14 <- as.factor(votingRecordsProcessed$V14)

votingRecordsProcessed$V15[votingRecordsProcessed$V15=="y"]<-"15Y"
votingRecordsProcessed$V15[votingRecordsProcessed$V15=="n"]<-"15N"
votingRecordsProcessed$V15[votingRecordsProcessed$V15=="?"]<-"15?"
votingRecordsProcessed$V15 <- as.factor(votingRecordsProcessed$V15)

votingRecordsProcessed$V16[votingRecordsProcessed$V16=="y"]<-"16Y"
votingRecordsProcessed$V16[votingRecordsProcessed$V16=="n"]<-"16N"
votingRecordsProcessed$V16[votingRecordsProcessed$V16=="?"]<-"16?"
votingRecordsProcessed$V16 <- as.factor(votingRecordsProcessed$V16)

votingRecordsProcessed$V17[votingRecordsProcessed$V17=="y"]<-"17Y"
votingRecordsProcessed$V17[votingRecordsProcessed$V17=="n"]<-"17N"
votingRecordsProcessed$V17[votingRecordsProcessed$V17=="?"]<-"17?"
votingRecordsProcessed$V17 <- as.factor(votingRecordsProcessed$V17)


summary(votingRecordsProcessed)
votingRecordsProcessed[1:20,]

votingBuckets <- as(votingRecordsProcessed, "transactions")
votingBuckets[1:20,]
summary(votingBuckets)

itemFrequencyPlot(votingBuckets, support = 0.1, cex.names = 0.8)

# Apriori
rules <- apriori(votingBuckets, parameter = list(support = 0.01, confidence = 0.8))

# General (without regard to party)
inspect(sort(rules, by = "confidence")[1:5])

# Democrat Voting Records
rulesDemA <- subset(rules, subset = rhs %in% "V1=democrat")
inspect(sort(rulesDemA, by = "confidence")[1:3])

#GOP Voting Records
rulesGOPA <- subset(rules, subset = rhs %in% "V1=republican")
inspect(sort(rulesGOPA, by = "confidence")[1:3])


#Eclat
itemsets <- eclat(votingBuckets, parameter = list(sup = 0.05, minlen = 3, maxlen = 15))

fsets <- eclat(votingBuckets, parameter = list(sup = 0.1, minlen = 3))
fsets.top10 <- sort(fsets)[1:10]
inspect(sort(fsets.top10))

# Democrat Voting Records
rulesDemE <- subset(itemsets, subset = items %in% "V1=democrat")
inspect(sort(rulesDemE, by = "support")[1:3])

#GOP Voting Records
rulesGOPE <- subset(itemsets, subset = items %in% "V1=republican")
inspect(sort(rulesGOPE, by = "support")[1:3])