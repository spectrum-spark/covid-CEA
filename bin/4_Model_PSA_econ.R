# PSA with just econ inputs

source("1_Model_Base.R")

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

runs <- 100



covidData_PSA <- data.frame()


### Calculate Costs and DALYs ########################################################################################


  for (i in 1:runs){
    param           <- betaParam(mean = dModerate["mean"], se = (dModerate["high"]-dModerate["low"])/(2*1.96))
    dModerate.psa   <- rbeta(1, param$alpha, param$beta)
    
    param           <- betaParam(mean = dSevere["mean"], se = (dSevere["high"]-dSevere["low"])/(2*1.96))
    dSevere.psa    <- rbeta(1, param$alpha, param$beta)
    
    param           <- betaParam(mean = dCritical["mean"], se = (dCritical["high"]-dCritical["low"])/(2*1.96))
    dCritical.psa   <- rbeta(1, param$alpha, param$beta)
    
    param           <- betaParam(mean = dPostacute["mean"], se = (dPostacute["high"]-dPostacute["low"])/(2*1.96))
    dPostacute.psa  <- rbeta(1, param$alpha, param$beta)
    
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
    
    nModerate.psa  <- runif(1, min = nModerate["low"],  max = nModerate["high"])
    nSevere.psa    <- runif(1, min = nSevere["low"],    max = nSevere["high"])
    nCritical.psa  <- runif(1, min = nCritical["low"],  max = nCritical["high"])
    nPostacute.psa <- runif(1, min = nPostacute["low"], max = nPostacute["high"])
    
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
  yldHomecare  <- nHomecare  * (dModerate.psa * nModerate.psa)
  yldAdmitWard <- nAdmitWard * (dModerate.psa * nModerate.psa + dSevere.psa * nSevere.psa + 
                                  dPostacute.psa * nPostacute.psa)
  yldAdmitICU  <- nAdmitICU  * (dModerate.psa * nModerate.psa + dSevere.psa * nSevere.psa + 
                                  dCritical.psa * nCritical.psa + dPostacute.psa * nPostacute.psa)
  yld          <- yldAsymptom + yldHomecare + yldAdmitWard + yldAdmitICU 
  
  # Calculate DALYs
  daly  <- yld + yll
  dalyU <- yld + yllU 
  
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
  
  costDoses   <-  ifelse(group=="A", nVaxDoses * (cVaxGroupA.psa + cDeliveryA.psa) * (1 + pVaxWaste),
                         ifelse(group=="B", nVaxDoses * (cVaxGroupB.psa + cDeliveryB.psa) * (1 + pVaxWaste),
                                ifelse(group=="C", nVaxDoses * (cVaxGroupC.psa + cDeliveryC.psa) * (1 + pVaxWaste),
                                       NA)))
  
  costDeath   <- nDeaths * cBodyBag
  
  costDisease <- costHome + costWard + costICU + costDeath 
  
  cost        <- costDisease + costDoses 
  

  dfTemp <- data.frame(group, popType, scenario, vaxCoverage, tpLevel, boostStart, immuneEscape, ageScenario, cost, daly)
  
  dfTemp <- dfTemp %>% 
    group_by(group, vaxCoverage, tpLevel, immuneEscape, ageScenario) %>% 
    mutate(daly0 = daly[1], cost0 = cost[1], iDaly = daly0 - daly, iCost = cost - cost0, icer = iCost/iDaly) %>% 
    unite(scenarioBoostStart, scenario, boostStart, sep = " at ", remove = FALSE) %>% 
    unite(scenarioImmuneEscape, scenario, immuneEscape, sep = ", immune escape ", remove = FALSE) %>% 
    unite(scenarioVaxCoverage, scenario, vaxCoverage, sep = ", coverage ", remove = FALSE) 
  
  covidData_PSA = rbind(covidData_PSA, dfTemp)
  
}


# write_csv(covidData_PSA, "data/covidData_PSA.csv")

# psaParam <- data.frame(dModerate.psa, dSevere.psa, dCritical.psa, cHomeGroupA.psa, cHomeGroupB.psa, 
#                        cHomeGroupC.psa, dPostacute.psa, cWardGroupA.psa, cWardGroupB.psa, cWardGroupC.psa, 
#                        cICUGroupA.psa, cICUGroupB.psa, cICUGroupC.psa, cDeliveryA.psa, cDeliveryB.psa, 
#                        cDeliveryC.psa, cVaxGroupA.psa, cVaxGroupB.psa, cVaxGroupC.psa, nModerate.psa, 
#                        nSevere.psa, nCritical.psa, nPostacute.psa)
### Remove all objects except data frames containing results
#rm(list=ls()[! ls() %in% c("covidData_Base", "covidData_OWSA", "covidData_PSA")])









