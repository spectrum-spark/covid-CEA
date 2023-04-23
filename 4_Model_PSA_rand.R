# PSA with bootstraped epi inputs

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
  filter(population.type=="Older" & immune.escape.starts=="1.50 yr" & transmission.potential.level=="low TP" & 
           (boosting.starts=="Never" | boosting.starts=="1.75 yr") & 
           (scenario=="No further boosting" | scenario=="High-risk boosting"))


### Calculate Costs and DALYs ########################################################################################

covidData_PSA <- data.frame()
runs <- 2000

  for (i in 1:runs){
    
    # Bootstrap one row each per scenario from 5000 replicates
    bootData <-  covidData %>% 
      filter(iteration == sample(1:5000, 1)) %>% 
      group_by(desc(scenario)) %>%
      slice_sample(n=1,replace = TRUE)
    
    # Number of vaccine doses
    nVaxDoses    <- bootData$nVaxDoses
    
    # Scenario identifiers
    popSize      <- bootData$population.size
    popType      <- bootData$population.type
    scenario     <- bootData$scenario
    vaxCoverage  <- bootData$X1st.year.vaccination.coverage
    tpLevel      <- bootData$transmission.potential.level
    boostStart   <- bootData$boosting.starts
    immuneEscape <- bootData$immune.escape.starts
    timePeriod   <- bootData$time.period
    ageScenario  <- bootData$ageScenario
    group        <- ifelse(bootData$population.type=="Older", "A",
                           ifelse(bootData$population.type=="Younger" & bootData$X1st.year.vaccination.coverage !="20.0%", "B",
                                  "C"))
    
    # Categories of COVID-19 health states and deaths
    nAsymptom   <- bootData$infections_all_ages             - bootData$symptomatic_infections_all_ages
    nHomecare   <- bootData$symptomatic_infections_all_ages - bootData$admissions_all_ages
    nAdmitWard  <- bootData$admissions_all_ages             - bootData$ICU_admissions_all_ages
    nAdmitICU   <- bootData$ICU_admissions_all_ages
    nOccupyWard <- bootData$ward_occupancy_all_ages
    nOccupyICU  <- bootData$ICU_occupancy_all_ages
    nDeaths     <- bootData$deaths_all_ages
    
    # Random draws from specified probability distribution
    # Beta
    param           <- betaParam(mean = dModerate["mean"], se = (dModerate["high"]-dModerate["low"])/(2*1.96))
    dModerate.psa   <- rbeta(1, param$alpha, param$beta)
    
    param           <- betaParam(mean = dSevere["mean"], se = (dSevere["high"]-dSevere["low"])/(2*1.96))
    dSevere.psa    <- rbeta(1, param$alpha, param$beta)
    
    param           <- betaParam(mean = dCritical["mean"], se = (dCritical["high"]-dCritical["low"])/(2*1.96))
    dCritical.psa   <- rbeta(1, param$alpha, param$beta)
    
    param           <- betaParam(mean = dPostacute["mean"], se = (dPostacute["high"]-dPostacute["low"])/(2*1.96))
    dPostacute.psa  <- rbeta(1, param$alpha, param$beta)
    
    # Gamma
    param           <- gammaParam(mean = cHomeGroupA["mean"], se = (cHomeGroupA["high"]-cHomeGroupA["low"])/(2*1.96))
    cHomeGroupA.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cHomeGroupB["mean"], se = (cHomeGroupB["high"]-cHomeGroupB["low"])/(2*1.96))
    cHomeGroupB.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cHomeGroupC["mean"], se = (cHomeGroupC["high"]-cHomeGroupC["low"])/(2*1.96))
    cHomeGroupC.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cWardGroupA["mean"], se = (cWardGroupA["high"]-cWardGroupA["low"])/(2*1.96))
    cWardGroupA.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cWardGroupB["mean"], se = (cWardGroupB["high"]-cWardGroupB["low"])/(2*1.96))
    cWardGroupB.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cWardGroupC["mean"], se = (cWardGroupC["high"]-cWardGroupC["low"])/(2*1.96))
    cWardGroupC.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cICUGroupA["mean"], se = (cICUGroupA["high"]-cICUGroupA["low"])/(2*1.96))
    cICUGroupA.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cICUGroupB["mean"], se = (cICUGroupB["high"]-cICUGroupB["low"])/(2*1.96))
    cICUGroupB.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cICUGroupC["mean"], se = (cICUGroupC["high"]-cICUGroupC["low"])/(2*1.96))
    cICUGroupC.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cDeliveryA["mean"], se = (cDeliveryA["high"]-cDeliveryA["low"])/(2*1.96))
    cDeliveryA.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cDeliveryB["mean"], se = (cDeliveryB["high"]-cDeliveryB["low"])/(2*1.96))
    cDeliveryB.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cDeliveryC["mean"], se = (cDeliveryC["high"]-cDeliveryC["low"])/(2*1.96))
    cDeliveryC.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cVaxGroupA["mean"], se = (cVaxGroupA["high"]-cVaxGroupA["low"])/(2*1.96))
    cVaxGroupA.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cVaxGroupB["mean"], se = (cVaxGroupB["high"]-cVaxGroupB["low"])/(2*1.96))
    cVaxGroupB.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    param           <- gammaParam(mean = cVaxGroupC["mean"], se = (cVaxGroupC["high"]-cVaxGroupC["low"])/(2*1.96))
    cVaxGroupC.psa <- rgamma(1, shape=param$shape, scale=param$scale)
    
    # Uniform
    nModerate.psa  <- runif(1, min = nModerate["low"],  max = nModerate["high"])
    nSevere.psa    <- runif(1, min = nSevere["low"],    max = nSevere["high"])
    nCritical.psa  <- runif(1, min = nCritical["low"],  max = nCritical["high"])
    nPostacute.psa <- runif(1, min = nPostacute["low"], max = nPostacute["high"])
    
    # Calculate Discounted YLLs by age groups and the total YLLs
    yll0.9   <- ifelse(popType=="Older", bootData$deaths0.9   * (1-exp(-dRate * lifeExpU[1]))/dRate, bootData$deaths0.9   * (1-exp(-dRate * lifeExpM[1]))/dRate)
    yll10.19 <- ifelse(popType=="Older", bootData$deaths10.19 * (1-exp(-dRate * lifeExpU[2]))/dRate, bootData$deaths10.19 * (1-exp(-dRate * lifeExpM[2]))/dRate)
    yll20.29 <- ifelse(popType=="Older", bootData$deaths20.29 * (1-exp(-dRate * lifeExpU[3]))/dRate, bootData$deaths20.29 * (1-exp(-dRate * lifeExpM[3]))/dRate)
    yll30.39 <- ifelse(popType=="Older", bootData$deaths30.39 * (1-exp(-dRate * lifeExpU[4]))/dRate, bootData$deaths30.39 * (1-exp(-dRate * lifeExpM[4]))/dRate)
    yll40.49 <- ifelse(popType=="Older", bootData$deaths40.49 * (1-exp(-dRate * lifeExpU[5]))/dRate, bootData$deaths40.49 * (1-exp(-dRate * lifeExpM[5]))/dRate)
    yll50.59 <- ifelse(popType=="Older", bootData$deaths50.59 * (1-exp(-dRate * lifeExpU[6]))/dRate, bootData$deaths50.59 * (1-exp(-dRate * lifeExpM[6]))/dRate)
    yll60.69 <- ifelse(popType=="Older", bootData$deaths60.69 * (1-exp(-dRate * lifeExpU[7]))/dRate, bootData$deaths60.69 * (1-exp(-dRate * lifeExpM[7]))/dRate)
    yll70.79 <- ifelse(popType=="Older", bootData$deaths70.79 * (1-exp(-dRate * lifeExpU[8]))/dRate, bootData$deaths70.79 * (1-exp(-dRate * lifeExpM[8]))/dRate)
    yll80.   <- ifelse(popType=="Older", bootData$deaths80.   * (1-exp(-dRate * lifeExpU[9]))/dRate, bootData$deaths80.   * (1-exp(-dRate * lifeExpM[9]))/dRate)
    yll      <- yll0.9 + yll10.19 + yll20.29 + yll30.39 + yll40.49 + yll50.59 + yll60.69 + yll70.79 + yll80.
    
    # Calculate YLDs by COVID categories and total YLDs
    yldAsymptom  <- 0
    yldHomecare  <- nHomecare  * (dModerate.psa * nModerate.psa)
    yldAdmitWard <- nAdmitWard * (dModerate.psa * nModerate.psa + dSevere.psa * nSevere.psa + 
                                    dPostacute.psa * nPostacute.psa)
    yldAdmitICU  <- nAdmitICU  * (dModerate.psa * nModerate.psa + dSevere.psa * nSevere.psa + 
                                    dCritical.psa * nCritical.psa + dPostacute.psa * nPostacute.psa)
    yld          <- yldAsymptom + yldHomecare + yldAdmitWard + yldAdmitICU 
    
    # Calculate DALYs
    daly  <- yld + yll
    
    # Calculating costs
    costHome    <- ifelse(group=="A", nHomecare * cHomeGroupA.psa, 
                          ifelse(group=="B", nHomecare * cHomeGroupB.psa,
                                 ifelse(group=="C", nHomecare * cHomeGroupC.psa,
                                        NA)))
    
    costWard    <- ifelse(group=="A", nOccupyWard * cWardGroupA.psa,
                          ifelse(group=="B", nOccupyWard * cWardGroupB.psa,
                                 ifelse(group=="C", nOccupyWard * cWardGroupC.psa,
                                        NA)))
    
    costICU     <- ifelse(group=="A", nOccupyICU * cICUGroupA.psa,
                          ifelse(group=="B", nOccupyICU * cICUGroupB.psa,
                                 ifelse(group=="C", nOccupyICU * cICUGroupC.psa,
                                        NA)))
    
    costDoses   <-  ifelse(group=="A", nVaxDoses * (cVaxGroupA.psa + cDeliveryA.psa) * (1 + pVaxWaste[1]),
                           ifelse(group=="B", nVaxDoses * (cVaxGroupB.psa + cDeliveryB.psa) * (1 + pVaxWaste[1]),
                                  ifelse(group=="C", nVaxDoses * (cVaxGroupC.psa + cDeliveryC.psa) * (1 + pVaxWaste[1]),
                                         NA)))
    
    costDeath   <- nDeaths * cBodyBag[1]
    
    costDisease <- costHome + costWard + costICU + costDeath 
    
    cost        <- costDisease + costDoses 
    
  
    dfTemp <- data.frame(group, popType, scenario, vaxCoverage, tpLevel, boostStart, immuneEscape, 
                         ageScenario, cost, daly)
    
    dfTemp <- dfTemp %>% 
      group_by(group, vaxCoverage, tpLevel, immuneEscape, ageScenario) %>% 
      mutate(daly0 = daly[1], cost0 = cost[1], iDaly = daly0 - daly, iCost = cost - cost0, icer = iCost/iDaly) %>% 
      unite(scenarioBoostStart, scenario, boostStart, sep = " at ", remove = FALSE) %>% 
      unite(scenarioImmuneEscape, scenario, immuneEscape, sep = ", immune escape ", remove = FALSE) %>% 
      unite(scenarioVaxCoverage, scenario, vaxCoverage, sep = ", coverage ", remove = FALSE) %>% 
      filter(iDaly!=0)
    
    covidData_PSA = rbind(covidData_PSA, dfTemp)
  
}

mean(covidData_PSA$icer)

#plot(covidData_PSA$iDaly, covidData_PSA$iCost)
# write_csv(covidData_PSA, "data/covidData_PSA.csv")


### Remove all objects except data frames containing results
#rm(list=ls()[! ls() %in% c("covidData_Base", "covidData_OWSA", "covidData_PSA")])












