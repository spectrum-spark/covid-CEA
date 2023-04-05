# Load required packages (install packages if necessary)
library(tidyverse)

rm(list=ls())


# Read CSV files needed for model
countryData <- read.csv("data/countryData.csv")
lifeTable   <- read.csv("data/lifeTable.csv")
vaxPrice    <- read.csv("data/vaxPrice.csv")




# Specify inputs and parameters (including base case, lower limit, upper limit)
dModerate  <- c(mean = 0.051, low = 0.032, high = 0.074) # disability weight for moderate health state
dSevere    <- c(mean = 0.133, low = 0.088, high = 0.190) # disability weight for severe health state
dCritical  <- c(mean = 0.675, low = 0.506, high = 0.822) # disability weight for critical health state
dPostacute <- c(mean = 0.219, low = 0.148, high = 0.308) # disability weight for post-acute consequences

nModerate  <- c(mean = 7/365,  low = 2/365, high = 9/365)  # number of days in moderate health state
nSevere    <- c(mean = 5/365,  low = 3/365, high = 9/365)  # number of days in severe health state
nCritical  <- c(mean = 7/365,  low = 4/365, high = 11/365) # number of days in critical health state
nPostacute <- c(mean = 14/365, low = 7/365, high = 28/365) # number of days with post-COVID symptoms

cHomeGroupA <- c(mean = mean(countryData$cHomeBased[c(1:4)], na.rm = TRUE), 
                 low  = min(countryData$cHomeBased[c(1:4)], na.rm = TRUE), 
                 high = max(countryData$cHomeBased[c(1:4)], na.rm = TRUE))
cWardGroupA <- c(mean = mean(countryData$cHospitalWard[c(1:4)], na.rm = TRUE), 
                 low  = min(countryData$cHospitalWard[c(1:4)], na.rm = TRUE), 
                 high = max(countryData$cHospitalWard[c(1:4)], na.rm = TRUE))
cICUGroupA  <- c(mean = mean(countryData$cHospitalICU[c(1:4)], na.rm = TRUE),
                 low  = min(countryData$cHospitalICU[c(1:4)], na.rm = TRUE),
                 high = max(countryData$cHospitalICU[c(1:4)], na.rm = TRUE))
cDeliveryA  <- c(mean = mean(countryData$cCoverage70[c(1:4)], na.rm = TRUE),
                 low  = min(countryData$cCoverage70[c(1:4)], na.rm = TRUE),
                 high = max(countryData$cCoverage70[c(1:4)], na.rm = TRUE)
)
cVaxGroupA  <- c(mean = vaxPrice$high[5],
                 low  = min(vaxPrice$high[c(1:4)]),
                 high = max(vaxPrice$high[c(1:4)])
)

cHomeGroupB <- c(mean = mean(countryData$cHomeBased[c(11:22)], na.rm = TRUE), 
                 low  = min(countryData$cHomeBased[c(11:22)], na.rm = TRUE), 
                 high = max(countryData$cHomeBased[c(11:22)], na.rm = TRUE)
)
cWardGroupB <- c(mean = mean(countryData$cHospitalWard[c(11:22)], na.rm = TRUE), 
                 low  = min(countryData$cHospitalWard[c(11:22)], na.rm = TRUE), 
                 high = max(countryData$cHospitalWard[c(11:22)], na.rm = TRUE)
)
cICUGroupB  <- c(mean = mean(countryData$cHospitalICU[c(11:22)], na.rm = TRUE),
                 low  = min(countryData$cHospitalICU[c(11:22)], na.rm = TRUE),
                 high = max(countryData$cHospitalICU[c(11:22)], na.rm = TRUE)
)
cICUGroupB <- cICUGroupB * 1.2
cDeliveryB  <- c(mean = mean(countryData$cCoverage70[c(11:18)], na.rm = TRUE),
                 low  = min(countryData$cCoverage70[c(11:18)], na.rm = TRUE),
                 high = max(countryData$cCoverage70[c(11:18)], na.rm = TRUE)
)
cVaxGroupB  <- c(mean = vaxPrice$lowerMiddle[5],
                 low  = min(vaxPrice$lowerMiddle, vaxPrice$upperMiddle),
                 high = max(vaxPrice$lowerMiddle, vaxPrice$upperMiddle)
)

cHomeGroupC <- c(mean = mean(countryData$cHomeBased[c(19:20)], na.rm = TRUE), 
                 low  = min(countryData$cHomeBased[c(19:20)], na.rm = TRUE), 
                 high = max(countryData$cHomeBased[c(19:20)], na.rm = TRUE)
)
cWardGroupC <- c(mean = mean(countryData$cHospitalWard[c(19:20)], na.rm = TRUE), 
                 low  = min(countryData$cHospitalWard[c(19:20)], na.rm = TRUE), 
                 high = max(countryData$cHospitalWard[c(19:20)], na.rm = TRUE)
)
cICUGroupC  <- c(mean = mean(countryData$cHospitalICU[c(19:20)], na.rm = TRUE),
                 low  = min(countryData$cHospitalICU[c(19:20)], na.rm = TRUE),
                 high = max(countryData$cHospitalICU[c(19:20)], na.rm = TRUE)
)
cICUGroupC <- cICUGroupC * 1.2
cDeliveryC  <- c(mean = mean(countryData$cCoverage20[c(19:20)], na.rm = TRUE),
                 low  = min(countryData$cCoverage20[c(19:20)], na.rm = TRUE),
                 high = max(countryData$cCoverage20[c(19:20)], na.rm = TRUE)
)
cVaxGroupC  <- c(mean = vaxPrice$lowerMiddle[5],
                 low  = min(vaxPrice$lowerMiddle[c(1:4)]),
                 high = max(vaxPrice$lowerMiddle[c(1:4)])
)

pVaxWaste <- c(mean = 0.10, low = 0.00, high = 0.20)
cBodyBag  <- c(mean = 64.5, low = 64.5, high = 64.5)
dRate     <- c(mean = 0.03, low = 0.03, high = 0.03)


cetWoodsA <- c(low=19000, high=30000)
cetWoodsB <- c(low=200,  high=1600)
cetWoodsC <- c(low=100,  high=1000)



input <- list("dModerate" = dModerate, "dSevere" = dSevere, "dCritical" = dCritical, "dPostacute" = dPostacute,
              "nModerate" = nModerate, "nSevere" = nSevere, "nCritical" = nCritical, "nPostacute" = nPostacute,
              "cHomeGroupA" = cHomeGroupA, "cWardGroupA" = cWardGroupA, "cICUGroupA" = cICUGroupA, "cDeliveryA" = cDeliveryA,
              "cVaxGroupA" = cVaxGroupA, "cHomeGroupB" = cHomeGroupB, "cWardGroupB" = cWardGroupB, "cICUGroupB" = cICUGroupB,
              "cDeliveryB" = cDeliveryB, "cVaxGroupB" = cVaxGroupB, "cHomeGroupC" = cHomeGroupC, "cWardGroupC" = cWardGroupC,
              "cICUGroupC" = cICUGroupC, "cDeliveryC" = cDeliveryC, "cVaxGroupC" = cVaxGroupC, "pVaxWaste" = pVaxWaste, 
              "cBodyBag" = cBodyBag)


# Calculate life expectancy by ten-year age groups (from five year age groups available)
lifeTable <- rbind(lifeTable, data.frame(ageGroup='00_09', t(colMeans(lifeTable[c(1:3), -1]))))
lifeTable <- rbind(lifeTable, data.frame(ageGroup='10_19', t(colMeans(lifeTable[c(4:5), -1]))))
lifeTable <- rbind(lifeTable, data.frame(ageGroup='20_29', t(colMeans(lifeTable[c(6:7), -1]))))
lifeTable <- rbind(lifeTable, data.frame(ageGroup='30_39', t(colMeans(lifeTable[c(8:9), -1]))))
lifeTable <- rbind(lifeTable, data.frame(ageGroup='40_49', t(colMeans(lifeTable[c(10:11), -1]))))
lifeTable <- rbind(lifeTable, data.frame(ageGroup='50_59', t(colMeans(lifeTable[c(12:13), -1]))))
lifeTable <- rbind(lifeTable, data.frame(ageGroup='60_69', t(colMeans(lifeTable[c(14:15), -1]))))
lifeTable <- rbind(lifeTable, data.frame(ageGroup='70_79', t(colMeans(lifeTable[c(16:17), -1]))))
lifeTable <- rbind(lifeTable, data.frame(ageGroup='80plus', t(colMeans(lifeTable[c(18:19), -1]))))

# Remove five-year life expectancies
lifeTable <- lifeTable[-c(1:19),]



### Clean Epi Data ########################################################################################

# Combine all epi data files into one
high_coverage <- read.csv("data/high_coverage_mean_clinical_outcomes_totals_1.5-3years.csv")
high_coverage$ageScenario <- "No"

low_coverage  <- read.csv("data/low_coverage_mean_clinical_outcomes_totals_1.5-3years.csv")
low_coverage$ageScenario <- "No"

many_boosters <- read.csv("data/many_boosters_high_coverage_mean_clinical_outcomes_totals_1.5-3years.csv")
many_boosters$ageScenario <- "No"

age_scenarios <- read.csv("data/age_scenarios_mean_clinical_outcomes_totals_1.5-3years.csv")
age_scenarios$ageScenario <- "Yes"

covidData     <- add_row(add_row(high_coverage, low_coverage), add_row(many_boosters, age_scenarios))


# More sensible contents
covidData[covidData == "TP_low"]                                <- "low TP"
covidData[covidData == "TP_high"]                               <- "high TP"
covidData[covidData == "80.0%"]                                 <- "80%"
covidData[covidData == "older"]                                 <- "Older"
covidData[covidData == "younger"]                               <- "Younger"
covidData[covidData == "never"]                                 <- "Never"
covidData[covidData == "1.5 (year)"]                            <- "1.50 yr"
covidData[covidData == "1.75 (year)"]                           <- "1.75 yr"
covidData[covidData == "2.0 (year)"]                            <- "2.00 yr"
covidData[covidData == "2.25 (year)"]                           <- "2.25 yr"
covidData[covidData == "2.5 (year)"]                            <- "2.50 yr"
covidData[covidData == "further boosting pediatric"]            <- "Pediatric boosting"
covidData[covidData == "further boosting random"]               <- "Random boosting"
covidData[covidData == "further boosting high risk"]            <- "High-risk boosting"
covidData[covidData == "no further boosting"]                   <- "No further boosting"
covidData[covidData == "high risk boosting"]                    <- "6-monthly boosting"
covidData[covidData == "further primary vaccination pediatric"] <- "Pediatric vaccination"
covidData[covidData == "further primary vaccination random"]    <- "Random vaccination"
covidData[covidData == "no further vaccination"]                <- "No further vaccination"


# Create new column for number of vaccine doses by scenarios
covidData$nVaxDoses <- 0
covidData <- covidData %>%
  mutate(nVaxDoses = replace(nVaxDoses, scenario == "Pediatric boosting",    11000),
         nVaxDoses = replace(nVaxDoses, scenario == "High-risk boosting",    11000),
         nVaxDoses = replace(nVaxDoses, scenario == "Random boosting",       11000),
         nVaxDoses = replace(nVaxDoses, scenario == "Pediatric vaccination", 11000),
         nVaxDoses = replace(nVaxDoses, scenario == "Random vaccination",    11000),
         nVaxDoses = replace(nVaxDoses, scenario == "6-monthly boosting",    33000),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boosting 65+", 11821),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boosting 55+", 21721),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boosting 45+", 32372),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boosting 35+", 42754),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boosting 25+", 53213),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boosting 16+", 61185),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boosting 5+",  70388),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boosting 65+", 3804),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boosting 55+", 9237),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boosting 45+", 16861),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boosting 35+", 26826),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boosting 25+", 39232),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boosting 16+", 51980),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boosting 5+",  70387))


### Objects needed for analysis ########################################################################################

# Number of vaccine doses
nVaxDoses    <- covidData$nVaxDoses

# Scenario identifiers
popSize      <- covidData$population.size
popType      <- covidData$population.type
scenario     <- covidData$scenario
vaxCoverage  <- covidData$X1st.year.vaccination.coverage
tpLevel      <- covidData$transmission.potential.level
boostStart   <- covidData$boosting.starts
immuneEscape <- covidData$immune.escape.starts
timePeriod   <- covidData$time.period
ageScenario  <- covidData$ageScenario
group        <- ifelse(covidData$population.type=="Older", "A",
                       ifelse(covidData$population.type=="Younger" & covidData$X1st.year.vaccination.coverage !="20.0%", "B",
                              "C"))

# Categories of COVID-19 health states and deaths
nAsymptom   <- covidData$total_mean_infections_all_ages             - covidData$total_mean_symptomatic_infections_all_ages
nHomecare   <- covidData$total_mean_symptomatic_infections_all_ages - covidData$total_mean_admissions_all_ages
nAdmitWard  <- covidData$total_mean_admissions_all_ages             - covidData$total_mean_ICU_admissions_all_ages
nAdmitICU   <- covidData$total_mean_ICU_admissions_all_ages
nOccupyWard <- covidData$total_mean_ward_occupancy_all_ages
nOccupyICU  <- covidData$total_mean_ICU_occupancy_all_ages
nDeaths     <- covidData$total_mean_deaths_ages_all_ages

# Life expectancies for upper income and lower-middle income
lifeExpU <- lifeTable$japan
lifeExpM <- lifeTable$lowerMiddle



### Calculate Costs and DALYs ########################################################################################

names(covidData) <- sub('total_mean_deaths_ages_', 'deaths', names(covidData))

model <- function(input){
  
  # Calculate undiscounted YLLs by age groups and the total YLLs
  yll0.9U   <- ifelse(popType=="Older", covidData$deaths0.9   * lifeExpU[1],  covidData$deaths0.9   * lifeExpM[1])
  yll10.19U <- ifelse(popType=="Older", covidData$deaths10.19 * lifeExpU[2],  covidData$deaths10.19 * lifeExpM[2])
  yll20.29U <- ifelse(popType=="Older", covidData$deaths20.29 * lifeExpU[3],  covidData$deaths20.29 * lifeExpM[3])
  yll30.39U <- ifelse(popType=="Older", covidData$deaths30.39 * lifeExpU[4],  covidData$deaths30.39 * lifeExpM[4])
  yll40.49U <- ifelse(popType=="Older", covidData$deaths40.49 * lifeExpU[5],  covidData$deaths40.49 * lifeExpM[5])
  yll50.59U <- ifelse(popType=="Older", covidData$deaths50.59 * lifeExpU[6],  covidData$deaths50.59 * lifeExpM[6])
  yll60.69U <- ifelse(popType=="Older", covidData$deaths60.69 * lifeExpU[7],  covidData$deaths60.69 * lifeExpM[7])
  yll70.79U <- ifelse(popType=="Older", covidData$deaths70.79 * lifeExpU[8],  covidData$deaths70.79 * lifeExpM[8])
  yll80.U   <- ifelse(popType=="Older", covidData$deaths80.   * lifeExpU[9],  covidData$deaths80.   * lifeExpM[9])
  yllU      <- yll0.9U + yll10.19U + yll20.29U + yll30.39U + yll40.49U + yll50.59U + yll60.69U + yll70.79U + yll80.U
  
  
  # Calculate Discounted YLLs by age groups and the total YLLs
  yll0.9   <- ifelse(popType=="Older", covidData$deaths0.9   * (1-exp(-dRate * lifeExpU[1]))/dRate, covidData$deaths0.9   * (1-exp(-dRate * lifeExpM[1]))/dRate)
  yll10.19 <- ifelse(popType=="Older", covidData$deaths10.19 * (1-exp(-dRate * lifeExpU[2]))/dRate, covidData$deaths10.19 * (1-exp(-dRate * lifeExpM[2]))/dRate)
  yll20.29 <- ifelse(popType=="Older", covidData$deaths20.29 * (1-exp(-dRate * lifeExpU[3]))/dRate, covidData$deaths20.29 * (1-exp(-dRate * lifeExpM[3]))/dRate)
  yll30.39 <- ifelse(popType=="Older", covidData$deaths30.39 * (1-exp(-dRate * lifeExpU[4]))/dRate, covidData$deaths30.39 * (1-exp(-dRate * lifeExpM[4]))/dRate)
  yll40.49 <- ifelse(popType=="Older", covidData$deaths40.49 * (1-exp(-dRate * lifeExpU[5]))/dRate, covidData$deaths40.49 * (1-exp(-dRate * lifeExpM[5]))/dRate)
  yll50.59 <- ifelse(popType=="Older", covidData$deaths50.59 * (1-exp(-dRate * lifeExpU[6]))/dRate, covidData$deaths50.59 * (1-exp(-dRate * lifeExpM[6]))/dRate)
  yll60.69 <- ifelse(popType=="Older", covidData$deaths60.69 * (1-exp(-dRate * lifeExpU[7]))/dRate, covidData$deaths60.69 * (1-exp(-dRate * lifeExpM[7]))/dRate)
  yll70.79 <- ifelse(popType=="Older", covidData$deaths70.79 * (1-exp(-dRate * lifeExpU[8]))/dRate, covidData$deaths70.79 * (1-exp(-dRate * lifeExpM[8]))/dRate)
  yll80.   <- ifelse(popType=="Older", covidData$deaths80.   * (1-exp(-dRate * lifeExpU[9]))/dRate, covidData$deaths80.   * (1-exp(-dRate * lifeExpM[9]))/dRate)
  yll      <- yll0.9 + yll10.19 + yll20.29 + yll30.39 + yll40.49 + yll50.59 + yll60.69 + yll70.79 + yll80.
  
  # Calculate YLDs by COVID categories and total YLDs
  yldAsymptom  <- 0
  yldHomecare  <- nHomecare  * (input$dModerate * input$nModerate)
  yldAdmitWard <- nAdmitWard * (input$dModerate * input$nModerate + input$dSevere * input$nSevere + input$dPostacute * input$nPostacute)
  yldAdmitICU  <- nAdmitICU  * (input$dModerate * input$nModerate + input$dSevere * input$nSevere + input$dCritical * input$nCritical + input$dPostacute * input$nPostacute)
  yld          <- yldAsymptom + yldHomecare + yldAdmitWard + yldAdmitICU 
  
  # Calculate DALYs
  daly  <- yld + yll
  dalyU <- yld + yllU 
  
  # Calculating costs
  costHome    <- ifelse(group=="A", nHomecare * input$cHomeGroupA, 
                        ifelse(group=="B", nHomecare * input$cHomeGroupB,
                               ifelse(group=="C", nHomecare * input$cHomeGroupC,
                                      NA)))
  
  costWard    <- ifelse(group=="A", nOccupyWard * input$cWardGroupA,
                      ifelse(group=="B", nOccupyWard * input$cWardGroupB,
                             ifelse(group=="C", nOccupyWard * input$cWardGroupC,
                                    NA)))
  
  costICU     <- ifelse(group=="A", nOccupyICU * input$cICUGroupA,
                      ifelse(group=="B", nOccupyICU * input$cICUGroupB,
                             ifelse(group=="C", nOccupyICU * input$cICUGroupC,
                                    NA)))
  
  costDoses   <-  ifelse(group=="A", nVaxDoses * (input$cVaxGroupA+input$cDeliveryA) * (1+input$pVaxWaste),
                        ifelse(group=="B", nVaxDoses * (input$cVaxGroupB+input$cDeliveryB) * (1+input$pVaxWaste),
                               ifelse(group=="C", nVaxDoses * (input$cVaxGroupC+input$cDeliveryC) * (1+input$pVaxWaste),
                                      NA)))
  
  costDeath   <- nDeaths * input$cBodyBag
  
  costDisease <- costHome + costWard + costICU + costDeath 
  
  cost        <- costDisease + costDoses 
  
  # This is what the function will return
  dfTemp <- data.frame(group, popType, scenario, vaxCoverage, tpLevel, boostStart, immuneEscape, ageScenario, 
                       nVaxDoses, nAsymptom, nHomecare, nAdmitWard, nAdmitICU, nOccupyWard, nOccupyICU, nDeaths,
                       costHome, costWard, costICU, costDeath, costDoses, costDisease, cost,
                       yldAsymptom, yldHomecare, yldAdmitWard, yldAdmitICU, yll, yllU, yld, daly, dalyU)
  
  dfTemp <- dfTemp %>% 
    group_by(group, vaxCoverage, tpLevel, immuneEscape, ageScenario) %>% 
    mutate(daly0 = daly[1], cost0 = cost[1], iDaly = daly0 - daly, iCost = cost - cost0, icer = iCost/iDaly) %>% 
    unite(scenarioBoostStart, scenario, boostStart, sep = " at ", remove = FALSE) %>% 
    unite(scenarioImmuneEscape, scenario, immuneEscape, sep = ", immune escape ", remove = FALSE) %>% 
    unite(scenarioVaxCoverage, scenario, vaxCoverage, sep = ", coverage ", remove = FALSE) 
  
  
  return(dfTemp)

}


### Conduct base case analysis
inputBase      <- lapply(input, '[[', 1)
covidData_Base <- model(inputBase)
write_csv(covidData_Base, "data/covidData_Base.csv")



### Conduct deterministic sensitivity analysis
parameters <- c("dModerate", "dSevere", "dCritical", "dPostacute", "nModerate", "nSevere", "nCritical", 
                "nPostacute",  "cHomeGroupA", "cWardGroupA", "cICUGroupA", "cDeliveryA", "cVaxGroupA", 
                "cHomeGroupB", "cWardGroupB", "cICUGroupB", "cDeliveryB", "cVaxGroupB", "cHomeGroupC", 
                "cWardGroupC", "cICUGroupC", "cDeliveryC", "cVaxGroupC", "pVaxWaste")

observations   <- length(covidData$scenario)
covidData_OWSA <- NULL
variables      <- c("group", "scenario", "vaxCoverage", "tpLevel", "boostStart", "immuneEscape", "icer")

for (i in 1:length(parameters)){
  param    <- parameters[i]
  inputVar <- inputBase
  
  inputVar[param] <- input[[param]]["low"]  # Change current "var" to the LOWER value for that param
  covidDataLow     <- model(inputVar)       # Run model for those params, store in output
  
  inputVar[param] <- input[[param]]["high"] # Change current "var" to the UPPER value for that param
  covidDataHigh    <- model(inputVar)       # Run model for those params, store in output
  
  covidData_OWSA <- rbind(covidData_OWSA, data.frame("result" = rep(c("Low", "High"),each=observations),
                                                     "parameter" = rep(param,2), 
                                                     rbind(covidDataLow[, variables], covidDataHigh[, variables])))
}


covidData_OWSA[covidData_OWSA == "cVaxGroupA"]  <- "Cost vaccine dose (A)"
covidData_OWSA[covidData_OWSA == "cVaxGroupB"]  <- "Cost vaccine dose (B)"
covidData_OWSA[covidData_OWSA == "cVaxGroupC"]  <- "Cost vaccine dose (C)"
covidData_OWSA[covidData_OWSA == "pVaxWaste"]   <- "Prop. doses wasted"
covidData_OWSA[covidData_OWSA == "cDeliveryA"]  <- "Cost vax delivery (A)"
covidData_OWSA[covidData_OWSA == "cDeliveryB"]  <- "Cost vax delivery (B)"
covidData_OWSA[covidData_OWSA == "cDeliveryC"]  <- "Cost vax delivery (C)"
covidData_OWSA[covidData_OWSA == "cHomeGroupA"] <- "Cost home-based (A)"
covidData_OWSA[covidData_OWSA == "cHomeGroupB"] <- "Cost home-based (B)"
covidData_OWSA[covidData_OWSA == "cHomeGroupC"] <- "Cost home-based (C)"
covidData_OWSA[covidData_OWSA == "cICUGroupA"]  <- "Cost bedday ICU (A)"
covidData_OWSA[covidData_OWSA == "cICUGroupB"]  <- "Cost bedday ICU (B)"
covidData_OWSA[covidData_OWSA == "cICUGroupC"]  <- "Cost bedday ICU (C)"
covidData_OWSA[covidData_OWSA == "cWardGroupA"] <- "Cost bedday ward (A)"
covidData_OWSA[covidData_OWSA == "cWardGroupB"] <- "Cost bedday ward (B)"
covidData_OWSA[covidData_OWSA == "cWardGroupC"] <- "Cost bedday ward (C)"
covidData_OWSA[covidData_OWSA == "dModerate"]   <- "Dis. weight moderate"
covidData_OWSA[covidData_OWSA == "dSevere"]     <- "Dis. weight severe"
covidData_OWSA[covidData_OWSA == "dCritical"]   <- "Dis. weight critical"
covidData_OWSA[covidData_OWSA == "dPostacute"]  <- "Dis. weight postacute"
covidData_OWSA[covidData_OWSA == "nModerate"]   <- "Days sick moderate"
covidData_OWSA[covidData_OWSA == "nSevere"]     <- "Days sick severe"
covidData_OWSA[covidData_OWSA == "nCritical"]   <- "Days sick critical"
covidData_OWSA[covidData_OWSA == "nPostacute"]  <- "Days sick postacute"

write_csv(covidData_OWSA, "data/covidData_OWSA.csv")

# 
# ### Remove all objects except data frames containing results
# rm(list=ls()[! ls() %in% c("covidData_Base","covidData_OWSA", "dModerate", "dSevere", "dCritical", "dPostacute", 
#                            "nModerate", "nSevere", "nCritical", "nPostacute", "cHomeGroupA", "cWardGroupA",
#                            "cICUGroupA", "cDeliveryA", "cVaxGroupA", "cHomeGroupB", "cWardGroupB", "cICUGroupB", 
#                            "cDeliveryB", "cVaxGroupB", "cHomeGroupC", "cWardGroupC", "cICUGroupC", "cDeliveryC",
#                            "cVaxGroupC", "pVaxWaste", "cBodyBag", "dRate", "cetWoodsA", "cetWoodsB", "cetWoodsC",
#                            "lifeTable", "vaxPrice", "covidData")])









