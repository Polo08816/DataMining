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




##Extra Data
# 5/10 bins (number is off for some reason) for ViolentCrimesPerPop
for(m in 1:2){
  n.bins <- m*10
  n.size<-length(crimeData$ViolentCrimesPerPop_numeric)
  whichbin <- crimeData
  #  print(whichbin)
  
  r.violentCrimes<-max(crimeData$ViolentCrimesPerPop_numeric) - min(crimeData$ViolentCrimesPerPop_numeric) + 1
  binwidth<-r.violentCrimes/n.bins
  print(binwidth)
  for (i in 1:n.bins){
    for(j in 1:n.size){
      if((i-1)*binwidth < crimeData$ViolentCrimesPerPop_numeric[j] && crimeData$ViolentCrimesPerPop_numeric[j] <= (i)*binwidth)
        whichbin$ViolentCrimesPerPop_numeric[j] <- i
      if((i == 1) && (crimeData$ViolentCrimesPerPop_numeric[j] == 0)) {
        whichbin$ViolentCrimesPerPop_numeric[j] <- i
      }
    }
  }
  print(whichbin$ViolentCrimesPerPop_numeric)
  hist(whichbin$ViolentCrimesPerPop_numeric,
       breaks = m*5,
       xlim = c(1,m*5),
       col = "lightblue",
       ylab = "Count",
       xlab = "Bin",
       main = "Histogram of Binned Violent Crimes Per Population")
  
  set.seed(1234)
  ind <- sample(2, n.size, replace=TRUE,
                prob=c(0.7,0.3))
  trainData <- whichbin[ind==1, ]
  testData <- whichbin[ind==2,]
  
  crimeData_rpart <- rpart(ViolentCrimesPerPop_numeric ~ race_percent_white_numeric + percent_with_investment_income_numeric + percent_not_high_school_grad_numeric + total_percent_divorced_numeric, data = trainData, method = "class")
  printcp(crimeData_rpart)
  plotcp(crimeData_rpart)
  plot(crimeData_rpart)
  text(crimeData_rpart, use.n=TRUE)
  crimeData_pred <- predict(crimeData_rpart, testData[,-6], type="class")
  print(crimeData_pred)
  print(table(crimeData_pred, testData$ViolentCrimesPerPop_numeric))
}





# Race
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$race_percent_black_numeric+
        crimeData$race_percent_white_numeric+
        crimeData$race_percent_asian_numeric+
        crimeData$race_percent_hispanic_numeric)

# Income
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_with_wage_numeric+
        crimeData$percent_with_farm_self_numeric+
        crimeData$percent_with_investment_income_numeric+
        crimeData$percent_with_social_security_numeric+
        crimeData$percent_with_public_assistance_numeric+
        crimeData$percent_with_retire_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_with_investment_income_numeric)

# More Income and Race
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$median_family_income_numeric+
        crimeData$per_capita_income_numeric+
        crimeData$white_per_capita_numeric+
        crimeData$black_per_capita_numeric+
        crimeData$indian_per_capita_numeric+
        crimeData$asian_per_capita_numeric+
        crimeData$other_per_capita_numeric+
        crimeData$hispanic_per_capitap_numeric)

# Education Level and Occupation
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_less_9th_grade_numeric+
        crimeData$percent_not_high_school_grad_numeric+
        crimeData$percent_bachelors_or_more_numeric+
        crimeData$percent_unemployed_numeric+
        crimeData$percent_employed_numeric+
        crimeData$percent_employed_manufacturing_numeric+
        crimeData$percent_employed_professional_service_numeric+
        crimeData$percent_occupation_manufacturing_numeric+
        crimeData$percent_occupation_management_professional_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_less_9th_grade_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_not_high_school_grad_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_unemployed_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_bachelors_or_more_numeric)

# Divorce
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$male_percent_divorced_numeric+
        crimeData$male_percent_never_married_numeric+
        crimeData$female_percent_divorced_numeric+
        crimeData$total_percent_divorced_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$male_percent_divorced_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$female_percent_divorced_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$total_percent_divorced_numeric)

# Family Makeup
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$person_per_family_numeric+
        crimeData$percent_family_2_parents_numeric+
        crimeData$percent_kids_2_parents_numeric+
        crimeData$percent_young_kids_2_parents_numeric+
        crimeData$percent_teen_2_parents_numeric+
        crimeData$percent_working_mom_young_kids_numeric+
        crimeData$percent_working_mom_numeric+
        crimeData$num_illegitimate_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_family_2_parents_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_kids_2_parents_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_young_kids_2_parents_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_teen_2_parents_numeric)

# Immigrant Information
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_immigrants_recent_numeric+
        crimeData$percent_immigrant_recent_5_numeric+
        crimeData$percent_immigrant_recent_8_numeric+
        crimeData$percent_immigrant_recent_10_numeric+
        crimeData$percent_recent_immigrant_numeric+
        crimeData$percent_recent_immigrant_5_numeric+
        crimeData$percent_recent_immigrant_8_numeric+
        crimeData$percent_recent_immigrant_10_numeric)

# English/Non-English
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_speak_english_only_numeric+
        crimeData$percent_not_speak_english_well_numeric)

# Household
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_large_household_family+
        crimeData$percent_large_household_occupied_numeric+
        crimeData$persons_per_occupied_household_numeric+
        crimeData$persons_per_owner_occupied_household_numeric+
        crimeData$persons_per_rent_occupied_household_numeric+
        crimeData$percent_person_owner_occupied_numeric+
        crimeData$percent_persons_dense_household_numeric+
        crimeData$percent_household_less_3_bedrooms_numeric+
        crimeData$median_number_bedrooms_numeric)
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_household_less_3_bedrooms_numeric)

# Neighborhood Vacancies/Households
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$households_vacant_numeric+
        crimeData$percent_households_occupied_numeric+
        crimeData$percent_household_owner_occupied_numeric+
        crimeData$percent_vacant_boarded_numeric+
        crimeData$percent_vacant_more_6_months_numeric)

# State of Housing
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$median_year_housing_built_numeric+
        crimeData$percent_household_no_phone_numeric+
        crimeData$percent_with_out_full_plumbing_numeric)

# Quartile
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$owner_occupied_low_quartile_numeric+
        crimeData$owner_occupied_median_value_numeric+
        crimeData$owner_occupied_high_quartile_numeric+
        crimeData$rent_low_quartile_numeric+
        crimeData$rent_median_numeric+
        crimeData$rent_high_quartile_numeric+
        crimeData$median_rent_numeric)

# More Cost Info
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$MedRentpercent_HousInc_numeric+
        crimeData$MedOwnCostpercent_Inc_numeric+
        crimeData$MedOwnCostpercent_IncNoMtg_numeric)

# Homeless
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$NumInShelters_numeric+
        crimeData$NumStreet_numeric)

# Area People From
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$percent_ForeignBorn_numeric+
        crimeData$percent_BornSameState_numeric+
        crimeData$percent_SameHouse85_numeric+
        crimeData$percent_SameCity85_numeric+
        crimeData$percent_SameState85_numeric)

# Lemas
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$LemasSwornFT_numeric+
        crimeData$LemasSwFTPerPop_numeric+
        crimeData$LemasSwFTFieldOps_numeric+
        crimeData$LemasSwFTFieldPerPop_numeric+
        crimeData$LemasTotalReq_numeric+
        crimeData$LemasTotReqPerPop_numeric)

# Police Makeup
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$PolicReqPerOffic_numeric+
        crimeData$PolicPerPop_numeric+
        crimeData$RacialMatchCommPol_numeric+
        crimeData$percent_PolicWhite_numeric+
        crimeData$percent_PolicBlack_numeric+
        crimeData$percent_PolicHisp_numeric+
        crimeData$percent_PolicAsian_numeric+
        crimeData$percent_PolicMinor_numeric)

# Police Jobs
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$OfficAssgnDrugUnits_numeric+
        crimeData$NumKindsDrugsSeiz_numeric+
        crimeData$PolicAveOTWorked_numeric)

# Land/Density/Public Transit
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$LandArea_numeric+
        crimeData$PopDens_numeric+
        crimeData$percent_UsePubTrans_numeric)

# More Police/Lemas
pairs(~crimeData$ViolentCrimesPerPop_numeric+
        crimeData$PolicCars_numeric+
        crimeData$PolicOperBudg_numeric+
        crimeData$Lemaspercent_PolicOnPatr_numeric+
        crimeData$LemasGangUnitDeploy_numeric+
        crimeData$Lemaspercent_OfficDrugUn_numeric+
        crimeData$PolicBudgPerPop_numeric)



hist(crimeData$ViolentCrimesPerPop_numeric,
     breaks = 5,
     xlim = c(0,1),
     col = "lightblue",
     ylab = "Count",
     xlab = "Violent Crimes Per Population",
     main = "Histogram of MPG")
plot(crimeData$number_under_poverty_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0.4,1),
     ylim = c(0.4,1),
     xlab = "Number Under Poverty",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by NUP",
     type = "p",
     pch = 16,
     col = "green")
points(crimeData$number_under_poverty_numeric,
       crimeData$ViolentCrimesPerPop_numeric,
       type = "p",
       col = "black")

plot(crimeData$female_percent_divorced_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0,1),
     ylim = c(0.4,1),
     xlab = "Female Percent Divorced",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by FPD",
     type = "p",
     pch = 16,
     col = "green")

plot(crimeData$percent_BornSameState_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0,1),
     ylim = c(0.4,1),
     xlab = "Percent Born Same State",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by PBSS",
     type = "p",
     pch = 16,
     col = "green")

plot(crimeData$male_percent_divorced_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0,1),
     ylim = c(0.4,1),
     xlab = "Male Percent Divorce",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by MPD",
     type = "p",
     pch = 16,
     col = "green")

plot(crimeData$percent_UsePubTrans_numeric,
     crimeData$ViolentCrimesPerPop_numeric,
     xlim = c(0,1),
     ylim = c(0.4,1),
     xlab = "Percent Use Public Transit",
     ylab = "Violent Crimes Per Population",
     main = "Scatterplot of VCPP by PUPT",
     type = "p",
     pch = 16,
     col = "green")


#crimeData[crimeData=="?"]<-"0"


