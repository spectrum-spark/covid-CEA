#method 1:5000
source("4_Model_PSA_5000.R")

#method 2:rand
source("4_Model_PSA_rand.R")

covidData_Base <- read.csv("data/covidData_Base.csv")

# Load required packages
library(ggplot2)   # for plots

library(cowplot)   # for nicer themes in plots
theme_set(theme_classic(base_family = "univers"))

library(showtext)  # for custom fonts in plots
font_add("univers",   "fonts/UniversRegular.ttf")
font_add("universB", "fonts/UniversBold.ttf")
showtext_auto()


## ggplot2 theme specifications for all figures
cetWoodsA <- c(low=19000, high=30000)
cetWoodsB <- c(low=200,  high=1600)
cetWoodsC <- c(low=100,  high=1000)

xlab <- xlab("DALYs averted per 100,000 pop")
ylab <- ylab("Incremental costs ($) per 100,000 pop")

units <- function(n) {
  labels <- ifelse(n < -1e9, paste0(round(n/1e6), 'M'),  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          paste0(round(n/1e6), 'M')  # in millions
                   ))
  return(labels)
}

xscale <- scale_x_continuous(breaks=seq(-50,  200,  25),  
                             limits = c(-50,  200))
yscale <- scale_y_continuous(breaks=seq(-450000, 750000, 150000), 
                             limits = c(-450000, 750000), 
                             labels=units)

xscale1 <- scale_x_continuous(breaks=seq(-50,  200,  25),  
                             limits = c(-50,  200))
yscale1 <- scale_y_continuous(breaks=seq(-450000, 1500000, 150000), 
                             limits = c(-450000, 1500000), 
                             labels=units)

hline  <- geom_hline(yintercept=0, linetype="solid", color = "black", linewidth=0.5)
vline  <- geom_vline(xintercept=0, linetype="solid", color = "black", linewidth=0.5)
border <- panel_border(color = "#444444", size = 0.3, linetype = 1)
theme  <- theme(axis.title        = element_text(size = 11), 
                axis.text         = element_text(size = 10,  color = "black"),
                axis.line         = element_line(linewidth = 0.01, color = "#444444"),
                axis.ticks        = element_line(linewidth = 0.2, color = "black"),
                axis.ticks.length = unit(0.1, "cm"),
                panel.grid.major  = element_line(linewidth = 0.2, colour = "gray97"),
                plot.title        = element_text(size = 13, colour = "black", 
                                                 margin=margin(t=5, b=5), hjust = 0.1),
                legend.position = c(1, 0), 
                legend.justification = c(1.1, 0), 
                legend.margin = margin(0, 0, 0, 0),
                #legend.spacing.x = unit(0, 'cm'),
                legend.spacing.y = unit(0, 'cm'),
                legend.background = element_rect(fill="transparent"),
                legend.text = element_text(size=8),   #change legend text font size
                legend.title = element_text(size=10), #change legend title font size
                legend.key.size = unit(0.4, 'cm'),      #change legend key size
                #legend.key.height = unit(1, 'cm'),    #change legend key height
                #legend.key.width = unit(1, 'cm')      #change legend key width
) 

cetLowerA  <- annotate("text", y = 700000, x = 50, size=3, label = "CET: 19,000", family = "univers")
cetHigherA <- annotate("text", y = 750000, x = 15, size=3, label = "CET: 30,000", family = "univers")

cetLowerB  <- annotate("text", y = 100000, x = 375, size=3, label = "CET: 200",    family = "univers")
cetHigherB <- annotate("text", y = 650000, x = 375, size=3, label = "CET: 1,600",  family = "univers")

cetLowerC  <- annotate("text", y = 100000, x = 375, size=3, label = "CET: 100",    family = "univers")
cetHigherC <- annotate("text", y = 650000, x = 375, size=3, label = "CET: 1,000",  family = "univers")





##high_coverage_scenarios_1_2_7_8_9_10

# Scenario 1: Group A, 80% coverage, immune escape at 1.5 years, high TP, all scenarios
df_Base <- covidData_Base %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="high TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="high TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Older population, 80% coverage, high TP, immune escape 1.5 yr, all scenarios"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA

ggsave(height=6, width=8, dpi=600, file="plots/PSA_01_03.pdf")


# Scenario 2: Group A, 80% coverage, immune escape at 2.5 years, high TP, all scenarios
df_Base <- covidData_Base %>% filter(group=="A" & immuneEscape=="2.50 yr" & tpLevel=="high TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA %>% filter(group=="A" & immuneEscape=="2.50 yr" & tpLevel=="high TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Older population, 80% coverage, high TP, immune escape 2.5 yr, all scenarios"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA

ggsave(height=6, width=8, dpi=600, file="plots/PSA_02_04.pdf")


# !!!Scenario 7: Group A, 80% coverage, immune escape at 1.5 years, low TP, all scenarios
df_Base <- covidData_Base %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="low TP"&
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="low TP"&
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Older population, 80% coverage, low TP, immune escape 1.5 yr, boosting starts 2.0 yr"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA

ggsave(height=6, width=8, dpi=600, file="plots/PSA_07_11.pdf")

# Scenario 8: Group A, 80% coverage, immune escape at 2.5 years, low TP, all scenarios
df_Base <- covidData_Base %>% filter(group=="A" & immuneEscape=="2.50 yr" & tpLevel=="low TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA %>% filter(group=="A" & immuneEscape=="2.50 yr" & tpLevel=="low TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Older population, 80% coverage, low TP, immune escape 2.5 yr, boosting starts 2.0 yr"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA

ggsave(height=6, width=8, dpi=600, file="plots/PSA_08_12.pdf")

# Scenario 9: Group B, 80% coverage, immune escape at 1.5 years, high TP, all scenarios
df_Base <- covidData_Base %>% filter(group=="B" & immuneEscape=="1.50 yr" & tpLevel=="high TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA %>% filter(group=="B" & immuneEscape=="1.50 yr" & tpLevel=="high TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Younger population, 80% coverage, high TP, immune escape 1.5 yr, all scenarios"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB

ggsave(height=6, width=8, dpi=600, file="plots/PSA_09.pdf")


# Scenario 10: Group B, 80% coverage, immune escape at 2.5 years, high TP, all scenarios!!!!
df_Base <- covidData_Base %>% filter(group=="B" & immuneEscape=="2.50 yr" & tpLevel=="high TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA %>% filter(group=="B" & immuneEscape=="2.50 yr" & tpLevel=="high TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Younger population, 80% coverage, high TP, immune escape 2.5 yr, all scenarios"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB

ggsave(height=6, width=8, dpi=600, file="plots/PSA_10.pdf")


##low_coverage_scenarios_5_6_13_14_15_16

# Scenario 5: Group B, 20% coverage, immune escape at 2 years, high TP, all scenarios
df_Base <- covidData_Base %>% filter(group=="C"& immuneEscape=="2.00 yr" & tpLevel=="high TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA %>% filter(group=="C" & immuneEscape=="2.00 yr" & tpLevel=="high TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Younger population, 20% coverage, high TP, immune escape 2.0 yr, all scenarios"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsC,  linewidth = 0.3, linetype="dashed") + cetLowerC + cetHigherC

ggsave(height=6, width=8, dpi=600, file="plots/PSA_5_15.pdf")

# Scenario 6: Group B, 50% coverage, immune escape at 2 years, high TP, all scenarios 
df_Base <- covidData_Base %>% filter(group=="B"& immuneEscape=="2.00 yr" & tpLevel=="high TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting") &
                                       (vaxCoverage=="50.0%"))
df_PSA  <- covidData_PSA %>% filter(group=="B" & immuneEscape=="2.00 yr" & tpLevel=="high TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting")&
                                      (vaxCoverage=="50.0%"))

ggtitle <- "Younger population, 50% coverage, high TP, immune escape 2.0 yr, all scenarios"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB

ggsave(height=6, width=8, dpi=600, file="plots/PSA_6_13.pdf")


# Scenario 13: Group B, 50% coverage, immune escape at 2 years, high TP, all scenarios: refer to scenario 6 

# Scenario 14: Group B, 50% coverage, immune escape at 2 years, low TP, all scenarios
df_Base <- covidData_Base %>% filter(group=="B" & immuneEscape=="2.00 yr" & tpLevel=="low TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting") &
                                       (vaxCoverage=="50.0%"))
df_PSA  <- covidData_PSA %>% filter(group=="B" & immuneEscape=="2.00 yr" & tpLevel=="low TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting")&
                                      (vaxCoverage=="50.0%"))

ggtitle <- "Younger population, 50% coverage, low TP, immune escape 2.0 yr, all scenarios"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB

ggsave(height=6, width=8, dpi=600, file="plots/PSA_14.pdf")


# Scenario 15: Group C, 20% coverage, immune escape at 2 years, high TP, all scenarios:refer to scenario 5  

# Scenario 16: Group C, 20% coverage, immune escape at 2 years, low TP, all scenarios 
df_Base <- covidData_Base %>% filter(group=="C"& immuneEscape=="2.00 yr" & tpLevel=="low TP"&
                                       (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA %>% filter(group=="C" & immuneEscape=="2.00 yr" & tpLevel=="low TP"&
                                      (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Younger population, 20% coverage, low TP, immune escape 2.0 yr, all scenarios"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsC,  linewidth = 0.3, linetype="dashed") + cetLowerC + cetHigherC

ggsave(height=6, width=8, dpi=600, file="plots/PSA_16.pdf")


##many_boosters_scenarios_3_4_11_12

# Scenario 3: Group A, 80% coverage, immune escape at 1.5 years, high TP, 6 monthly boosting: refer to scenario 1
# Scenario 4: Group A, 80% coverage, immune escape at 2.5 years, high TP, 6 monthly boosting: refer to scenario 2
# Scenario 11: Group B, 80% coverage, immune escape at 1.5 years, high TP, 6 monthly boosting: refer to scenario 7
# Scenario 12: Group B, 80% coverage, immune escape at 2.5 years, high TP, 6 monthly boosting: refer to scenario 8






