#Both epi and econ sensitivity

source("0_Inputs_Base.R")

betaParam <- function(mean, se) {
  alpha <- ((1 - mean) / se ^ 2 - 1 / mean) * mean ^ 2
  beta  <- alpha * (1 / mean - 1)
  params <- list(alpha = alpha, beta = beta)
  return(params)
}

gammaParam <- function(mean, se) {
  shape <- (mean ^ 2) / (se ^ 2)
  scale <- (se ^ 2) / mean
  params <- list(shape = shape, scale = scale)
}



### Clean Epi Data ########################################################################################

# Combine all epi data files into one
high_coverage_all <- read.csv("data/high_coverage_scenarios_1_2_7_8_9_10_ALL_clinical_outcomes_totals_1.5-3years.csv")
high_coverage_all$ageScenario <- "No"
low_coverage_all <- read.csv("data/low_coverage_scenarios_5_6_13_14_15_16_ALL_clinical_outcomes_totals_1.5-3years.csv")
low_coverage_all$ageScenario <- "No"
many_boosters_all <- read.csv("data/many_boosters_scenarios_3_4_11_12_high_coverage_ALL_clinical_outcomes_totals_1.5-3years.csv")
many_boosters_all$ageScenario <- "No"

covidData   <- add_row(add_row(high_coverage_all, low_coverage_all), many_boosters_all)
rm(high_coverage_all, low_coverage_all, many_boosters_all)


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

names(covidData) <- sub('total_deaths_ages_', 'deaths', names(covidData))
names(covidData) <- sub('total_', '', names(covidData))


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

write_csv(covidData, "data/covidData_All.csv")



covidData <- read_csv("data/covidData_All.csv")

covidData  <- covidData  %>%
  filter(population.type=="Older" & immune.escape.starts=="1.50 yr" & transmission.potential.level=="high TP" & 
           (boosting.starts=="Never" | boosting.starts=="2.00 yr") & 
           (scenario=="No further boosting" | scenario=="High-risk boosting"))


### Calculate Costs and DALYs ########################################################################################

covidData_PSA <- data.frame()

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
  iteration   <- covidData$iteration
  
  # Categories of COVID-19 health states and deaths
  nAsymptom   <- covidData$infections_all_ages             - covidData$symptomatic_infections_all_ages
  nHomecare   <- covidData$symptomatic_infections_all_ages - covidData$admissions_all_ages
  nAdmitWard  <- covidData$admissions_all_ages             - covidData$ICU_admissions_all_ages
  nAdmitICU   <- covidData$ICU_admissions_all_ages
  nOccupyWard <- covidData$ward_occupancy_all_ages
  nOccupyICU  <- covidData$ICU_occupancy_all_ages
  nDeaths     <- covidData$deaths_all_ages

  
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
  yldHomecare  <- nHomecare  * (dModerate["mean"] * nModerate["mean"])
  yldAdmitWard <- nAdmitWard * (dModerate["mean"] * nModerate["mean"] + dSevere["mean"] * nSevere["mean"] + 
                                  dPostacute["mean"] * nPostacute["mean"])
  yldAdmitICU  <- nAdmitICU  * (dModerate["mean"] * nModerate["mean"] + dSevere["mean"] * nSevere["mean"] + 
                                  dCritical["mean"] * nCritical["mean"] + dPostacute["mean"] * nPostacute["mean"])
  yld          <- yldAsymptom + yldHomecare + yldAdmitWard + yldAdmitICU 
  
  # Calculate DALYs
  daly  <- yld + yll
  
  # Calculating costs
  costHome    <- ifelse(group=="A", nHomecare * cHomeGroupA["mean"], 
                        ifelse(group=="B", nHomecare * cHomeGroupB["mean"],
                               ifelse(group=="C", nHomecare * cHomeGroupC["mean"],
                                      NA)))
  
  costWard    <- ifelse(group=="A", nOccupyWard * cWardGroupA["mean"],
                        ifelse(group=="B", nOccupyWard * cWardGroupB["mean"],
                               ifelse(group=="C", nOccupyWard * cWardGroupC["mean"],
                                      NA)))
  
  costICU     <- ifelse(group=="A", nOccupyICU * cICUGroupA["mean"],
                        ifelse(group=="B", nOccupyICU * cICUGroupB["mean"],
                               ifelse(group=="C", nOccupyICU * cICUGroupC["mean"],
                                      NA)))
  
  costDoses   <-  ifelse(group=="A", nVaxDoses * (cVaxGroupA["mean"] + cDeliveryA["mean"]) * (1 + pVaxWaste[1]),
                         ifelse(group=="B", nVaxDoses * (cVaxGroupB["mean"] + cDeliveryB["mean"]) * (1 + pVaxWaste[1]),
                                ifelse(group=="C", nVaxDoses * (cVaxGroupC["mean"] + cDeliveryC["mean"]) * (1 + pVaxWaste[1]),
                                       NA)))
  
  costDeath   <- nDeaths * cBodyBag[1]
  
  costDisease <- costHome + costWard + costICU + costDeath 
  
  cost        <- costDisease + costDoses 
  
  
  dfTemp <- data.frame(group, popType, scenario, iteration, vaxCoverage, tpLevel, boostStart, immuneEscape, 
                       ageScenario, yld, yll, daly, costDeath, costDisease, cost)
  
  dfTemp <- dfTemp %>%
    group_by(iteration) %>%
    mutate(daly0 = daly[1], cost0 = cost[1], iDaly = daly0 - daly, iCost = cost - cost0) %>%
    unite(scenarioBoostStart, scenario, boostStart, sep = " at ", remove = FALSE) %>%
    unite(scenarioImmuneEscape, scenario, immuneEscape, sep = ", immune escape ", remove = FALSE) %>%
    unite(scenarioVaxCoverage, scenario, vaxCoverage, sep = ", coverage ", remove = FALSE) %>% 
    filter(iDaly!=0)
  


icostPSA <- mean(dfTemp$iCost)
idalyPSA <- mean(dfTemp$iDaly)
icerPSA <- icostPSA / idalyPSA

print(c("iCost", round(icostPSA,0)))
print(c("iDaly", round(idalyPSA,0)))
print(c("icer", round(icerPSA,0)))


plot(covidData_PSA$iDaly, covidData_PSA$iCost)


ggplot(dfTemp) +
  geom_point(aes(x=iDaly,       y=iCost),       shape=21, size=2.5, 
             stroke=0.5, fill="#FFFFFFEE", color="deepskyblue4") +
  geom_hline(yintercept=0, linetype="solid", color = "black", size=0.5) +
  geom_vline(xintercept=0, linetype="solid", color = "black", size=0.5) +
  theme_bw()


