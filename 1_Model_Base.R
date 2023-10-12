source("0_Inputs_Base.R")

### Clean Epi Data ########################################################################################

# Combine all epi data files into one
high_coverage <- read.csv("data/high_coverage_scenarios_1_2_7_8_9_10_mean_clinical_outcomes_totals_1.5-3years.csv")
high_coverage$ageScenario <- "No"

low_coverage <- read.csv("data/low_coverage_scenarios_5_6_13_14_15_16_mean_clinical_outcomes_totals_1.5-3years.csv")
low_coverage$ageScenario <- "No"

many_boosters <- read.csv("data/many_boosters_scenarios_3_4_11_12_high_coverage_mean_clinical_outcomes_totals_1.5-3years.csv")
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
covidData[covidData == "further boosting pediatric"]            <- "Pediatric boost"
covidData[covidData == "further boosting random"]               <- "Random boost"
covidData[covidData == "further boosting high risk"]            <- "High-risk boost"
covidData[covidData == "no further boosting"]                   <- "No further boost"
covidData[covidData == "high risk boosting"]                    <- "6-monthly boost"
covidData[covidData == "further primary vaccination pediatric"] <- "Pediatric vax"
covidData[covidData == "further primary vaccination random"]    <- "Random vax"
covidData[covidData == "no further vaccination"]                <- "No further vax"
covidData[covidData == "boosting 65+"]                          <- "boost 65+"
covidData[covidData == "boosting 55+"]                          <- "boost 55+"
covidData[covidData == "boosting 45+"]                          <- "boost 45+"
covidData[covidData == "boosting 35+"]                          <- "boost 35+"
covidData[covidData == "boosting 25+"]                          <- "boost 25+"
covidData[covidData == "boosting 16+"]                          <- "boost 16+"
covidData[covidData == "boosting 5+"]                           <- "boost 5+"


# Create new column for number of vaccine doses by scenarios
covidData$nVaxDoses <- 0
covidData <- covidData %>%
  mutate(nVaxDoses = replace(nVaxDoses, scenario == "Pediatric boost",    11000),
         nVaxDoses = replace(nVaxDoses, scenario == "High-risk boost",    11000),
         nVaxDoses = replace(nVaxDoses, scenario == "Random boost",       11000),
         nVaxDoses = replace(nVaxDoses, scenario == "Pediatric vax", 11000),
         nVaxDoses = replace(nVaxDoses, scenario == "Random vax",    11000),
         nVaxDoses = replace(nVaxDoses, scenario == "6-monthly boost",    33000),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boost 65+", 11821),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boost 55+", 21721),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boost 45+", 32372),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boost 35+", 42754),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boost 25+", 53213),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boost 16+", 61185),
         nVaxDoses = replace(nVaxDoses, population.type  == "Older"   & scenario == "boost 5+",  70388),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boost 65+", 3804),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boost 55+", 9237),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boost 45+", 16861),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boost 35+", 26826),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boost 25+", 39232),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boost 16+", 51980),
         nVaxDoses = replace(nVaxDoses, population.type  == "Younger" & scenario == "boost 5+",  70387))


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
 
  # Scenario analysis ($0 vaccine dose)
  costDoses0   <-  ifelse(group=="A", nVaxDoses * (input$cDeliveryA) * (1+input$pVaxWaste),
                          ifelse(group=="B", nVaxDoses * (input$cDeliveryB) * (1+input$pVaxWaste),
                                 ifelse(group=="C", nVaxDoses * (input$cDeliveryC) * (1+input$pVaxWaste),
                                        NA)))

  
  costDeath   <- nDeaths * input$cBodyBag
  
  costDisease <- costHome + costWard + costICU + costDeath 
  
  cost        <- costDisease + costDoses 
  
  costDonated        <- costDisease + costDoses0
  
  # Scenario analysis (no home-based visits)
  costnoHome        <- costDisease + costDoses - costHome
  
  # This is what the function will return
  dfTemp <- data.frame(group, popType, scenario, vaxCoverage, tpLevel, boostStart, immuneEscape, ageScenario, 
                       nVaxDoses, nAsymptom, nHomecare, nAdmitWard, nAdmitICU, nOccupyWard, nOccupyICU, nDeaths,
                       costHome, costWard, costICU, costDeath, costDoses, costDoses0, costDonated, costDisease, costnoHome, cost,
                       yldAsymptom, yldHomecare, yldAdmitWard, yldAdmitICU, yll, yllU, yld, daly, dalyU)
  
  dfTemp <- dfTemp %>% 
    group_by(group, vaxCoverage, tpLevel, immuneEscape, ageScenario) %>% 
    mutate(daly0 = daly[1], cost0 = cost[1], iDaly = daly0 - daly, iCost = cost - cost0, icer = iCost/iDaly, 
           iCostDonated = costDonated - cost0, icerDonated = iCostDonated/iDaly,
           costnoHome0 = costnoHome[1] , iCostnoHome = costnoHome - costnoHome0, icernoHome = iCostnoHome/iDaly) %>% 
    unite(scenarioBoostStart, scenario, boostStart, sep = " at ", remove = FALSE) %>% 
    unite(scenarioImmuneEscape, scenario, immuneEscape, sep = ", immune esc ", remove = FALSE) %>% 
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
variables      <- c("group", "scenario", "vaxCoverage", "tpLevel", "boostStart", "immuneEscape", "icer", "icerDonated")

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

covidData_Base <- read_csv("data/covidData_Base.csv")

test  <- covidData_Base  %>%
  filter(popType=="Older" & immuneEscape=="1.50 yr" & tpLevel=="low TP" & 
           (boostStart=="Never" | boostStart=="1.75 yr") & 
           (scenario=="No further boost" | scenario=="High-risk boost"))



### Test CE consistency across different scenarios
# testCE <- covidData_Base %>%
#   mutate(
#     NHB = case_when(
#       group == "A" ~ 30000 * iDaly - iCost,
#       group == "B" ~ 1600 * iDaly - iCost,
#       group == "C" ~ 1000 * iDaly - iCost,
#       TRUE ~ NA_real_ 
#     ),
#     NHBdonated = case_when(
#       group == "A" ~ 30000 * iDaly - iCostDonated,
#       group == "B" ~ 1600 * iDaly - iCostDonated,
#       group == "C" ~ 1000 * iDaly - iCostDonated,
#       TRUE ~ NA_real_  
#     ),
#     NHBnohome = case_when(
#       group == "A" ~ 30000 * iDaly - iCostnoHome,
#       group == "B" ~ 1600 * iDaly - iCostnoHome,
#       group == "C" ~ 1000 * iDaly - iCostnoHome,
#       TRUE ~ NA_real_  
#     )
#   ) %>%
#   mutate(
#     CE_all_consis = ifelse((NHB > 0 & NHBdonated > 0 & NHBnohome > 0) | (NHB < 0 & NHBdonated < 0 & NHBnohome < 0) | (NHB == 0 & NHBdonated == 0 & NHBnohome == 0),
#                        "yes", "no"),
#     CE_donated_consis = ifelse((NHB > 0 & NHBdonated > 0) | (NHB < 0 & NHBdonated < 0) | (NHB == 0 & NHBdonated == 0),
#                            "yes", "no"),
#     CE_nohome_consis = ifelse((NHB > 0 & NHBnohome > 0) | (NHB < 0 & NHBnohome < 0) | (NHB == 0 & NHBnohome == 0),
#                            "yes", "no"),
#   )







