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
dRate     <- 0.03


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

# Life expectancies for upper income and lower-middle income
lifeExpU <- lifeTable$japan
lifeExpM <- lifeTable$lowerMiddle
