#### Input data set plants scale into Data Frame "plantsData"
#crimeData <- read.csv(file = "C:/Users/Kevin Kuo/git/DataMining/GroupProject/UCI/communities.data",
#crimeData <- read.csv(file = "C:/Users/J14688/git/DataMining/GroupProject/UCI/communities.data",
crimeData <- read.csv(file = "C:/Users/maryjoyce/git/COSC757/GroupProject/UCI/communities.data",
                      header = FALSE, sep = ",", stringsAsFactors = TRUE,
                      col.names = c("state_numeric", "county_numeric", "community_numeric", "community_name_string", "fold_numeric",
                                    "population_numeric", "household_size_numeric", "race_percent_black_numeric", "race_percent_white_numeric",
                                    "race_percent_asian_numeric", "race_percent_hispanic_numeric", "age_percent_12_to_21_numeric",
                                    "age_percent_12_to_29_numeric", "age_percent_16_to_24_numeric", "age_percent_65_up_numeric",
                                    "number_urban_numeric", "percent_urban_numeric", "median_income_numeric", "percent_with_wage_numeric",
                                    "percent_with_farm_self_numeric", "percent_with_investment_income_numeric", "percent_with_social_security_numeric",
                                    "percent_with_public_assistance_numeric", "percent_with_retire_numeric", "median_family_income_numeric",
                                    "per_capita_income_numeric", "white_per_capita_numeric", "black_per_capita_numeric", "indian_per_capita_numeric",
                                    "asian_per_capita_numeric", "other_per_capita_numeric", "hispanic_per_capitap_numeric", "number_under_poverty_numeric",
                                    "percent_population_under_poverty_numeric", "percent_less_9th_grade_numeric", "percent_not_high_school_grad_numeric",
                                    "percent_bachelors_or_more_numeric", "percent_unemployed_numeric", "percent_employed_numeric",
                                    "percent_employed_manufacturing_numeric", "percent_employed_professional_service_numeric",
                                    "percent_occupation_manufacturing_numeric", "percent_occupation_management_professional_numeric",
                                    "male_percent_divorced_numeric", "male_percent_never_married_numeric", "female_percent_divorced_numeric",
                                    "total_percent_divorced_numeric", "person_per_family_numeric","percent_family_2_parents_numeric",
                                    "percent_kids_2_parents_numeric","percent_young_kids_2_parents_numeric", "percent_teen_2_parents_numeric",
                                    "percent_working_mom_young_kids_numeric", "percent_working_mom_numeric", "num_illegitimate_numeric",
                                    "percent_illegitimate_numeric", "number_immigrants_numeric", "percent_immigrants_recent_numeric",
                                    "percent_immigrant_recent_5_numeric", "percent_immigrant_recent_8_numeric", "percent_immigrant_recent_10_numeric",
                                    "percent_recent_immigrant_numeric", "percent_recent_immigrant_5_numeric", "percent_recent_immigrant_8_numeric",
                                    "percent_recent_immigrant_10_numeric", "percent_speak_english_only_numeric", "percent_not_speak_english_well_numeric",
                                    "percent_large_household_family","percent_large_household_occupied_numeric", "persons_per_occupied_household_numeric",
                                    "persons_per_owner_occupied_household_numeric", "persons_per_rent_occupied_household_numeric",
                                    "percent_person_owner_occupied_numeric", "percent_persons_dense_household_numeric", "percent_household_less_3_bedrooms_numeric",
                                    "median_number_bedrooms_numeric", "households_vacant_numeric", "percent_households_occupied_numeric",
                                    "percent_household_owner_occupied_numeric", "percent_vacant_boarded_numeric", "percent_vacant_more_6_months_numeric",
                                    "median_year_housing_built_numeric", "percent_household_no_phone_numeric", "percent_with_out_full_plumbing_numeric",
                                    "owner_occupied_low_quartile_numeric", "owner_occupied_median_value_numeric", "owner_occupied_high_quartile_numeric",
                                    "rent_low_quartile_numeric", "rent_median_numeric", "rent_high_quartile_numeric", "median_rent_numeric",
                                    "MedRentpercent_HousInc_numeric", "MedOwnCostpercent_Inc_numeric", "MedOwnCostpercent_IncNoMtg_numeric", "NumInShelters_numeric", "NumStreet_numeric",
                                    "percent_ForeignBorn_numeric","percent_BornSameState_numeric", "percent_SameHouse85_numeric", "percent_SameCity85_numeric",
                                    "percent_SameState85_numeric", "LemasSwornFT_numeric", "LemasSwFTPerPop_numeric", "LemasSwFTFieldOps_numeric",
                                    "LemasSwFTFieldPerPop_numeric", "LemasTotalReq_numeric", "LemasTotReqPerPop_numeric", "PolicReqPerOffic_numeric",
                                    "PolicPerPop_numeric", "RacialMatchCommPol_numeric", "percent_PolicWhite_numeric", "percent_PolicBlack_numeric",
                                    "percent_PolicHisp_numeric", "percent_PolicAsian_numeric", "percent_PolicMinor_numeric", "OfficAssgnDrugUnits_numeric",
                                    "NumKindsDrugsSeiz_numeric", "PolicAveOTWorked_numeric", "LandArea_numeric", "PopDens_numeric",
                                    "percent_UsePubTrans_numeric", "PolicCars_numeric", "PolicOperBudg_numeric", "Lemaspercent_PolicOnPatr_numeric",
                                    "LemasGangUnitDeploy_numeric", "Lemaspercent_OfficDrugUn_numeric", "PolicBudgPerPop_numeric", "ViolentCrimesPerPop_numeric"))

#crimeData[1:20,]

#crimeData[crimeData=="?"]<-0

#install.packages("rpart")
#library(rpart)
# install.packages("e1071")
# library(class)
# library(e1071)
# install.packages("randomForest")
# library(randomForest)
# library("arules", lib.loc="~/R/win-library/3.2")
# library(datasets)



binningFunct <- function(mNum, dataOrig, dataNew){
  n.bins <- mNum*5
  n.size<-length(dataOrig)
  #  print(whichbin)
  
  binwidth<-1/n.bins
  print(binwidth)
  for (i in 1:n.bins){
    for(j in 1:n.size){
      if((i-1)*binwidth < dataOrig[j] && dataOrig[j] <= (i)*binwidth)
        dataNew[j] <- i
      if((i == 1) && (dataOrig[j] == 0)) {
        dataNew[j] <- i
      }
    }
  }
  print(dataNew)
  hist(dataNew,
       breaks = (n.bins+1),
       xlim = c(0,(n.bins+1)),
       col = "lightblue",
       ylab = "Count",
       xlab = "Bin",
       main = "Histogram of A vs B")
  return(dataNew)
}

# Assumed to matter
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$PolicPerPop_numeric+
        crimeData$per_capita_income_numeric+
        crimeData$percent_bachelors_or_more_numeric)

# Determined to matter
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$race_percent_white_numeric+
        crimeData$percent_with_investment_income_numeric+
        crimeData$percent_not_high_school_grad_numeric+
        crimeData$total_percent_divorced_numeric)

#Scatterplots
plot(crimeData$race_percent_white_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Race Percent White",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by RPW",
     type = "p",
     pch = 16,
     col = "green")
points(crimeData$race_percent_white_numeric,
       crimeData$ViolentCrimesPerPop_numeric,
       type = "p",
       col = "black")
plot(crimeData$percent_with_investment_income_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Percent with Investment Incomce",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by PwII",
     type = "p",
     pch = 16,
     col = "green")
points(crimeData$percent_with_investment_income_numeric,
       crimeData$ViolentCrimesPerPop_numeric,
       type = "p",
       col = "black")
plot(crimeData$percent_not_high_school_grad_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Percent Not High School Graduates",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by PNHSG",
     type = "p",
     pch = 16,
     col = "green")
points(crimeData$percent_not_high_school_grad_numeric,
       crimeData$ViolentCrimesPerPop_numeric,
       type = "p",
       col = "black")
plot(crimeData$total_percent_divorced_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Total Percent Divorced",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by TPD",
     type = "p",
     pch = 16,
     col = "green")
points(crimeData$total_percent_divorced_numeric,
       crimeData$ViolentCrimesPerPop_numeric,
       type = "p",
       col = "black")


# 5/10 bins (number is off for some reason) for ViolentCrimesPerPop
for(m in 1:2) {
  whichbinViolentCrimes <- crimeData
  whichbinViolentCrimes$ViolentCrimesPerPop_numeric <- binningFunct(m, crimeData$ViolentCrimesPerPop_numeric, whichbinViolentCrimes$ViolentCrimesPerPop_numeric)

  n.size<- length(crimeData$ViolentCrimesPerPop_numeric)
  set.seed(1234)
  ind <- sample(2, n.size, replace=TRUE,
                prob=c(0.7,0.3))
  trainData <- whichbinViolentCrimes[ind==1, ]
  testData <- whichbinViolentCrimes[ind==2,]
  
  # classification
  crimeData_rpart <- rpart(ViolentCrimesPerPop_numeric ~ race_percent_white_numeric + percent_with_investment_income_numeric + percent_not_high_school_grad_numeric + total_percent_divorced_numeric, data = trainData, method = "class")
  printcp(crimeData_rpart)
  plotcp(crimeData_rpart)
  plot(crimeData_rpart)
  text(crimeData_rpart, use.n=TRUE)
  crimeData_pred <- predict(crimeData_rpart, testData[,-6], type="class")
  print(crimeData_pred)
  print(table(crimeData_pred, testData$ViolentCrimesPerPop_numeric))
  
  #classifier <- naiveBayes(ViolentCrimesPerPop_numeric ~ race_percent_white_numeric + percent_with_investment_income_numeric + percent_not_high_school_grad_numeric + total_percent_divorced_numeric, data = trainData, method = "class")
  #print(classifier)
  #pred <- predict(classifier, testData[,-5])
  #print(table(pred))
  #print(table(testData$ViolentCrimesPerPop_numeric))
  #print(table(pred,testData$ViolentCrimesPerPop_numeric))
  #length(pred)
  #length(testData$ViolentCrimesPerPop_numeric)
  
  # fit <- randomForest(class ~ balanceData, data = trainData)
  #fit <- randomForest(ViolentCrimesPerPop_numeric ~ race_percent_white_numeric + percent_with_investment_income_numeric + percent_not_high_school_grad_numeric + total_percent_divorced_numeric, data = trainData, method = "class")
  #print(fit)
  #print(importance(fit))

    ### Regression Analysis
  nonall_crime<-crimeData$ViolentCrimesPerPop_numeric[!is.na(crimeData$ViolentCrimesPerPop_numeric) & !is.na(crimeData$race_percent_white_numeric) & !is.na(crimeData$percent_with_investment_income_numeric) & !is.na(crimeData$percent_not_high_school_grad_numeric) & !is.na(crimeData$total_percent_divorced_numeric)]
  length(nonall_crime)
  nonall_percent_white<-crimeData$race_percent_white_numeric[!is.na(crimeData$ViolentCrimesPerPop_numeric) & !is.na(crimeData$race_percent_white_numeric) & !is.na(crimeData$percent_with_investment_income_numeric) & !is.na(crimeData$percent_not_high_school_grad_numeric) & !is.na(crimeData$total_percent_divorced_numeric)]
  length(nonall_percent_white)
  nonall_investment<-crimeData$percent_with_investment_income_numeric[!is.na(crimeData$ViolentCrimesPerPop_numeric) & !is.na(crimeData$race_percent_white_numeric) & !is.na(crimeData$percent_with_investment_income_numeric) & !is.na(crimeData$percent_not_high_school_grad_numeric) & !is.na(crimeData$total_percent_divorced_numeric)]
  length(nonall_investment)
  nonall_not_high_school<-crimeData$percent_not_high_school_grad_numeric[!is.na(crimeData$ViolentCrimesPerPop_numeric) & !is.na(crimeData$race_percent_white_numeric) & !is.na(crimeData$percent_with_investment_income_numeric) & !is.na(crimeData$percent_not_high_school_grad_numeric) & !is.na(crimeData$total_percent_divorced_numeric)]
  length(nonall_not_high_school)
  nonall_divorce<-crimeData$total_percent_divorced_numeric[!is.na(crimeData$ViolentCrimesPerPop_numeric) & !is.na(crimeData$race_percent_white_numeric) & !is.na(crimeData$percent_with_investment_income_numeric) & !is.na(crimeData$percent_not_high_school_grad_numeric) & !is.na(crimeData$total_percent_divorced_numeric)]
  length(nonall_divorce)
  
  fit.percent_white<-lm(nonall_crime ~ nonall_percent_white)
  summary(fit.percent_white)
  fit.investment<-lm(nonall_crime ~ nonall_investment)
  summary(fit.investment)
  fit.high_school<-lm(nonall_crime ~ nonall_not_high_school)
  summary(fit.high_school)
  fit.divorce<-lm(nonall_crime ~ nonall_divorce)
  summary(fit.divorce)
  
  hypothesis<-cbind(nonall_crime,
                    nonall_percent_white,
                    nonall_investment,
                    nonall_not_high_school,
                    nonall_divorce)
  percentWhiteTest<-cor.test(nonall_crime,
                           nonall_percent_white)
  investmentTest<-cor.test(nonall_crime,
                       nonall_investment)
  highSchoolTest<-cor.test(nonall_crime,
                           nonall_not_high_school)
  divorceTest<-cor.test(nonall_crime,
                        nonall_divorce)
  crimeTest1<-cor.test(nonall_percent_white,
                      nonall_investment)
  crimeTest2<-cor.test(nonall_percent_white,
                       nonall_not_high_school)
  crimeTest3<-cor.test(nonall_percent_white,
                       nonall_divorce)
  crimeTest4<-cor.test(nonall_investment,
                       nonall_not_high_school)
  crimeTest5<-cor.test(nonall_investment,
                       nonall_divorce)
  crimeTest6<-cor.test(nonall_not_high_school,
                    nonall_divorce)
  round(cor(hypothesis),
        4)
  print(percentWhiteTest$p.value)
  print(investmentTest$p.value)
  print(highSchoolTest$p.value)
  print(divorceTest$p.value)
  print(crimeTest1$p.value)
  print(crimeTest2$p.value)
  print(crimeTest3$p.value)
  print(crimeTest4$p.value)
  print(crimeTest5$p.value)
  print(crimeTest6$p.value)
  
}


# 5 Bin for frequent itemsets
for(q in 1:1) {

  newBin <- crimeData
  newBin$ViolentCrimesPerPop_numeric <- binningFunct(q, crimeData$ViolentCrimesPerPop_numeric, newBin$ViolentCrimesPerPop_numeric)
  newBin$race_percent_white_numeric <- binningFunct(q, crimeData$race_percent_white_numeric, newBin$race_percent_white_numeric)
  newBin$percent_with_investment_income_numeric <- binningFunct(q, crimeData$percent_with_investment_income_numeric, newBin$percent_with_investment_income_numeric)
  newBin$percent_not_high_school_grad_numeric <- binningFunct(q, crimeData$percent_not_high_school_grad_numeric, newBin$percent_not_high_school_grad_numeric)
  newBin$total_percent_divorced_numeric <- binningFunct(q, crimeData$total_percent_divorced_numeric, newBin$total_percent_divorced_numeric)

# Frequent itemsets pre-processing
newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="1"]<-"1V"
newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="2"]<-"2V"
newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="3"]<-"3V"
newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="4"]<-"4V"
newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="5"]<-"5V"
if(q == 2) {
  newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="6"]<-"6V"
  newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="7"]<-"7V"
  newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="8"]<-"8V"
  newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="9"]<-"9V"
  newBin$ViolentCrimesPerPop_numeric[newBin$ViolentCrimesPerPop_numeric=="10"]<-"10V"
}
newBin$ViolentCrimesPerPop_numeric <- as.factor(newBin$ViolentCrimesPerPop_numeric)
newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="1"]<-"1A"
newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="2"]<-"2A"
newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="3"]<-"3A"
newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="4"]<-"4A"
newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="5"]<-"5A"
if(q == 2){
  newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="6"]<-"6A"
  newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="7"]<-"7A"
  newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="8"]<-"8A"
  newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="9"]<-"9A"
  newBin$race_percent_white_numeric[newBin$race_percent_white_numeric=="10"]<-"10A"
}
newBin$race_percent_white_numeric <- as.factor(newBin$race_percent_white_numeric)
newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="1"]<-"1B"
newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="2"]<-"2B"
newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="3"]<-"3B"
newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="4"]<-"4B"
newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="5"]<-"5B"
if(q == 2) {
  newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="6"]<-"6B"
  newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="7"]<-"7B"
  newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="8"]<-"8B"
  newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="9"]<-"9B"
  newBin$percent_with_investment_income_numeric[newBin$percent_with_investment_income_numeric=="10"]<-"10B"
}
newBin$percent_with_investment_income_numeric <- as.factor(newBin$percent_with_investment_income_numeric)
newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="1"]<-"1C"
newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="2"]<-"2C"
newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="3"]<-"3C"
newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="4"]<-"4C"
newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="5"]<-"5C"
if(q == 2){
  newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="6"]<-"6C"
  newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="7"]<-"7C"
  newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="8"]<-"8C"
  newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="9"]<-"9C"
  newBin$percent_not_high_school_grad_numeric[newBin$percent_not_high_school_grad_numeric=="10"]<-"10C"
}
newBin$percent_not_high_school_grad_numeric <- as.factor(newBin$percent_not_high_school_grad_numeric)
newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="1"]<-"1D"
newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="2"]<-"2D"
newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="3"]<-"3D"
newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="4"]<-"4D"
newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="5"]<-"5D"
if(q == 2){
  newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="6"]<-"6D"
  newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="7"]<-"7D"
  newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="8"]<-"8D"
  newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="9"]<-"9D"
  newBin$total_percent_divorced_numeric[newBin$total_percent_divorced_numeric=="10"]<-"10D"
}
newBin$total_percent_divorced_numeric <- as.factor(newBin$total_percent_divorced_numeric)
newBin[1:20,]

keeps <- c("ViolentCrimesPerPop_numeric","race_percent_white_numeric","percent_with_investment_income_numeric","percent_not_high_school_grad_numeric","total_percent_divorced_numeric")
keepNewBin = newBin[keeps]
keepNewBin[1:20,]

# Inspect the dataset
votingBaskets <- as(keepNewBin,"transactions")
print(summary(votingBaskets))
votingBaskets[1:2,]
# plot the data
itemFrequencyPlot(votingBaskets, support=0.1, cex.names=0.8)
itemFrequencyPlot(votingBaskets, support=0.01, cex.names=0.8)


## Apriori Algorithm
# association rules
rules <- apriori(votingBaskets, parameter = list(support=0.1, confidence=0.6, minlen=2))
# subset of rules
rulesV1 <- subset(rules, subset=rhs%in%"ViolentCrimesPerPop_numeric=1V")
inspect(sort(rulesV1, by="confidence")[1:5])
rulesV1L <- subset(rules, subset=lhs%in%"ViolentCrimesPerPop_numeric=1V")
inspect(sort(rulesV1L, by="confidence")[1:5])


## Eclat Algorithm
itemsets <- eclat(votingBaskets, parameter = list(sup=0.1, minlen=3, maxlen=15))
fsets <- eclat(votingBaskets, parameter=list(sup=0.1, minlen=3))
fsets.top5 <- sort(fsets)[1:5]
print(inspect(fsets.top5))
fsets.top10 <- sort(fsets)[1:10]
print(inspect(fsets.top10))
rulesV1E <- subset(itemsets, subset=items%in%"ViolentCrimesPerPop_numeric=1V")
print(inspect(sort(rulesV1E, by="support")[1:5]))

rules2 <- apriori(votingBaskets, parameter = list(support=0.1, confidence=0.6, minlen=3, maxlen=15))
rules2V1 <- subset(rules2, subset=rhs%in%"ViolentCrimesPerPop_numeric=1V")
inspect(sort(rules2V1, by="support")[1:5])
rules2V1L <- subset(rules2, subset=lhs%in%"ViolentCrimesPerPop_numeric=1V")
inspect(sort(rules2V1L, by="support")[1:5])
}






