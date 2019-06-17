library("ggplot2")
library("datasets")
library("dplyr")
library("missForest")
library("moments")
library("data.table")
library("Hmisc")
library("rms")
library("corrgram")
library("hydroGOF")
library("e1071")
library("rpart")
library('rminer')
library("randomForest")
library("fBasics")
setwd('C:\\users\\zeemo\\Desktop\\Files\\Data\\RStudio') 
crime = read.csv("crimedata.csv")


#Renaming
crime <- rename(crime, communityname = Êcommunityname)

#Removing dupicate records
crime <- unique(crime)

#Transfer variables starting population to numeric
crime[,6:ncol(crime)] <- sapply(crime[,6:ncol(crime)], as.character)
crime[,6:ncol(crime)] <- sapply(crime[,6:ncol(crime)], as.numeric)

#Table checks for NAs
na_table <- as.data.frame(!is.na(crime))
colMeans(na_table)

#Remove variables with more 50% NAs, now 125 variables, 22 removed
drop.cols <- c('LemasSwornFT','LemasSwFTPerPop','LemasSwFTFieldOps','LemasSwFTFieldPerPop',
               'LemasTotalReq','LemasTotReqPerPop','PolicReqPerOffic','PolicPerPop','RacialMatchCommPol',
               'PctPolicWhite','PctPolicBlack','PctPolicHisp','PctPolicAsian','PctPolicMinor',
               'OfficAssgnDrugUnits','NumKindsDrugsSeiz','PolicAveOTWorked','PolicCars',
               'PolicOperBudg','LemasPctPolicOnPatr','LemasGangUnitDeploy','PolicBudgPerPop')
crime <- crime %>% select(- drop.cols)

#householdsize and PersPerOccupHous represents the same info - remove one
crime$householdsize
crime$PersPerOccupHous
crime <- crime %>% select(-PersPerOccupHous)

#Data Preparation and Exploration
#remove State, County, Community, Community name, and fold from the dataset
crime_model <- crime %>% select(-communityname, -state, -countyCode, -communityCode, -fold)
#remove observations that contain NAs 
crime_ViolentModel <- crime_model[complete.cases(crime_model[,119]),]
#Removing the crime category, we want to know the relations for all kind crime rate not a specific one
drop.OtherPredictors <- c('murders','murdPerPop','rapes','rapesPerPop','robberies','robbbPerPop',
                          'assaults','assaultPerPop','burglaries','burglPerPop','larcenies','larcPerPop',
                          'autoTheft','autoTheftPerPop','arsons','arsonsPerPop','nonViolPerPop')
crime_ViolentModel <- crime_ViolentModel %>% select(-drop.OtherPredictors)

#Missing value - using missForest package
crime_MissingValueTreatment <- missForest(crime_ViolentModel)
#Check imputation error
crime_MissingValueTreatment$OOBerror
#The NRMSE is normalized root means squared error - 1.81% as 0.01817133 
crime_Violent_Imp <- as.data.frame(crime_MissingValueTreatment$ximp)

#Test
#info https://help.gooddata.com/doc/en/reporting-and-dashboards/maql-analytical-query-language/maql-expression-reference/aggregation-functions/statistical-functions/predictive-statistical-use-cases/normality-testing-skewness-and-kurtosis
#Normarlity
#There are both graphical and statistical methods for evaluating normality
#Graphical methods include the histogram and normality plot
#Statistically, two numerical measures of shape - skewness and excess kurtosis 
#Shapiro-Wilk Test for normality
#The Shapiro-Wilk test the null hypothesis
lshap <- lapply(crime_Violent_Imp, shapiro.test)
sshap <- sapply(lshap, `[`, c("statistic","p.value"))
Shapiro_Wilk <- as.data.frame(t(sshap))
Shapiro_Wilk <- Shapiro_Wilk %>% select(Shapiro_Wilk_pValue = p.value)
Shapiro_Wilk <- setDT(Shapiro_Wilk, keep.rownames = TRUE)[]
#PctWorkMomYoungKids 0.0729556620203055 failied the normality test
par(mfrow=c(2,2))
hist(crime_Violent_Imp$PctWorkMomYoungKids, main = "Histogram")
plot(density(crime_Violent_Imp$PctWorkMomYoungKids), main = "Density Plot")
boxplot(crime_Violent_Imp$PctWorkMomYoungKids, main = "Boxplot")
qqnorm(crime_Violent_Imp$PctWorkMomYoungKids, main = "Normality Plot")
qqline(crime_Violent_Imp$PctWorkMomYoungKids)

#Test for skewness
#If skewness is not close to zero, then your data set is not normally distributed.
#Calculating Skewness
lskew <- lapply(crime_Violent_Imp, skewness)
sskew <- sapply(lskew, `[`)
Skewness <- as.data.frame(sskew)
Skewness <- Skewness %>% select(Skewness = sskew)
Skewness <- setDT(Skewness, keep.rownames = TRUE)[]
#PctHousOccup is -3.51912021,   
#The left tail is longer; distribution is concentrated on the right and is left-skewed

par(mfrow=c(2,2))
hist(crime_Violent_Imp$PctHousOccup, main = "Histogram")
plot(density(crime_Violent_Imp$PctHousOccup), main = "Density Plot")
boxplot(crime_Violent_Imp$PctHousOccup, main = "Box Plot")
qqnorm(crime_Violent_Imp$PctHousOccup, main = "Normality Plot")
qqline(crime_Violent_Imp$PctHousOccup)
#NumStreet (Number of homeless people counted on the streets)=35.36923785 is most skewed towards the right
#lots communities have no visualization cuz they do not have lot homeless people on the street
#big city like new york or san jose resulted in the high numbers of that

#Test for kurtosis
#Kurtosis shows height and sharpness, middle of the curve
#info https://codeburst.io/2-important-statistics-terms-you-need-to-know-in-data-science-skewness-and-kurtosis-388fef94eeaa
#Calculating Kurtosis
lkurt <- lapply(crime_Violent_Imp, kurtosis)
skurt <- sapply(lkurt, `[`)
Kurtosis <- as.data.frame(skurt)
Kurtosis <- Kurtosis %>% select(Kurtosis = skurt)
Kurtosis <- setDT(Kurtosis, keep.rownames = TRUE)[]
#pctUrban (Percentage of people living in urban areas)is platykurtic distribution
#not a normal distribution
par(mfrow=c(2,2))
hist(crime_Violent_Imp$pctUrban, main = "Histogram")
plot(density(crime_Violent_Imp$pctUrban), main = "Density Plot")
boxplot(crime_Violent_Imp$pctUrban, main = "Box Plot")
qqnorm(crime_Violent_Imp$pctUrban, main = "Normality Plot")
qqline(crime_Violent_Imp$pctUrban)

#MedOwnCostPctIncNoMtg represents the most mesokurtic distribution which is normal distribution
#However, it doesn't pass the Shapiro-Wilk normality test?
#MedOwnCostPctIncNoMtg = 6.18103013741066e-31.I think it does pass the test
par(mfrow=c(2,2))
hist(crime_Violent_Imp$MedOwnCostPctIncNoMtg, main = "Histogram")
plot(density(crime_Violent_Imp$MedOwnCostPctIncNoMtg), main = "Density Plot")
boxplot(crime_Violent_Imp$MedOwnCostPctIncNoMtg, main = "Box Plot")
qqnorm(crime_Violent_Imp$MedOwnCostPctIncNoMtg, main = "Normality Plot")
qqline(crime_Violent_Imp$MedOwnCostPctIncNoMtg)

#summary 
#Combining all tests into a table
tests_df <- merge(x = Shapiro_Wilk, y = Skewness, by = "rn", all = FALSE)
tests_df <- merge(x = tests_df, y = Kurtosis, by = "rn", all = FALSE)
tests_df <- tests_df %>% rename(Variable = rn)
