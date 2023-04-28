source("1_Model_Base.R")

# Load required packages
library(tidyverse) # data management package
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

xscale <- scale_x_continuous(breaks=seq(-40,  400,  40),  
                             limits = c(-40,  400))
yscale <- scale_y_continuous(breaks=seq(-450000, 750000, 150000), 
                             limits = c(-450000, 750000), 
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

cetLowerA  <- annotate("text", y = 100000, x = 375, size=3, label = "CET = $19,000", family = "univers")
cetHigherA <- annotate("text", y = 650000, x = 375, size=3, label = "CET = $30,000", family = "univers")

cetLowerA1  <- annotate("text", y = 500000, x = 35, size=3, label = "CET = $19,000", family = "univers")
cetHigherA1 <- annotate("text", y = 650000, x = 20, size=3, label = "CET = $30,000", family = "univers")

cetLowerB  <- annotate("text", y = 100000, x = 375, size=3, label = "CET = $200",    family = "univers")
cetHigherB <- annotate("text", y = 650000, x = 375, size=3, label = "CET = $1,600",  family = "univers")

# cetLowerC  <- annotate("text", y = 100000, x = 375, size=3, label = "CET = $100",    family = "univers")
# cetHigherC <- annotate("text", y = 650000, x = 375, size=3, label = "CET = $1,000",  family = "univers")



## Style guide for points on scatter plots
# use filled shapes for immune escape at 1.5y and plus/open for 2.5y
# use plus/open shapes for "low TP" and filled shape for "high TP" scenarios
# filled shapes: circle, square, triangle, diamond
# their plus counterparts: circle plus, square plus, triangle square, diamond plus
# their open counterparts: circle open, square open, triangle open, diamond open
# other shapes: asterisk, cross, plus


# Scenario 1: Group A, 80% coverage, immune escape at 1.5 years, high TP, all scenarios (fig 8) 
df <- covidData_Base %>% filter(group=="A" & tpLevel=="high TP" & boostStart=="2.00 yr" & 
                                  (immuneEscape=="1.50 yr" | immuneEscape=="2.50 yr"))
ggtitle <- "Scenario: older population, 80% initial vaccination coverage, high TP, boosting 2 yr"

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle open", "square", "square open", "triangle", "triangle open")) +
  scale_color_manual(values=c("purple2","purple2", "darkcyan", "darkcyan", "red3", "red3")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA1 + cetHigherA1

ggsave(height=6, width=8, dpi=600, file="plots/figure_8.pdf")



# Scenario 3: Group A, 80% coverage, immune escape at 1.5 years, high TP, all scenarios (fig 11)
df <- covidData_Base %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
# Update the 6-monthly name
df <- df %>%
  mutate(scenarioBoostStart = replace(scenarioBoostStart, scenarioBoostStart == "6-monthly boosting at 1.75 yr", "6-monthly boosting"))

ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n high TP, immune escape 1.5 yr"

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("asterisk","circle", "square", "triangle", "diamond")) +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border +theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA1 + cetHigherA1

ggsave(height=6, width=8, dpi=600, file="plots/figure_11.pdf")




# Scenario 4: Group A, 80% coverage, immune escape at 2.5 years, high TP, all scenarios (fig 12)
df <- covidData_Base %>% filter(group=="A" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

# Update the 6-monthly name
df <- df %>%
  mutate(scenarioBoostStart = replace(scenarioBoostStart, scenarioBoostStart == "6-monthly boosting at 1.75 yr", "6-monthly boosting"))

ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n high TP, immune escape 2.5 yr"


ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("asterisk","circle open", "square open", "triangle open", "diamond open")) +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA1 + cetHigherA1

ggsave(height=6, width=8, dpi=600, file="plots/figure_12.pdf")



# Scenario 5 & 6: Group B/C, 20%/50% coverage, immune escape at 2 years, high TP, 6 monthly boosting (fig 16)

df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.00 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boosting" | scenario=="Pediatric vaccination" | 
                                     scenario=="Random vaccination") & boostStart=="2.00 yr" & 
                                  (vaxCoverage=="20.0%" | vaxCoverage=="50.0%"))

df <- df %>%
  mutate(scenarioVaxCoverage = replace(scenarioVaxCoverage, scenarioVaxCoverage == "Pediatric vaccination, coverage 20.0%", "Pediatric vaccination, initial coverage 20.0%")) %>%
  mutate(scenarioVaxCoverage = replace(scenarioVaxCoverage, scenarioVaxCoverage == "High-risk boosting, coverage 20.0%", "High-risk boosting, initial coverage 20.0%")) %>%
  mutate(scenarioVaxCoverage = replace(scenarioVaxCoverage, scenarioVaxCoverage == "Random vaccination, coverage 20.0%", "Random vaccination, initial coverage 20.0%")) %>%
  mutate(scenarioVaxCoverage = replace(scenarioVaxCoverage, scenarioVaxCoverage == "Pediatric vaccination, coverage 50.0%", "Pediatric vaccination, initial coverage 50.0%")) %>%
  mutate(scenarioVaxCoverage = replace(scenarioVaxCoverage, scenarioVaxCoverage == "High-risk boosting, coverage 50.0%", "High-risk boosting, initial coverage 50.0%")) %>%
  mutate(scenarioVaxCoverage = replace(scenarioVaxCoverage, scenarioVaxCoverage == "Random vaccination, coverage 50.0%", "Random vaccination, initial coverage 50.0%"))

ggtitle <- "Scenario: younger population, high TP, immune escape 2.0 yr, boosting starts 2.0 yr"


ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioVaxCoverage, color=scenarioVaxCoverage)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square", "triangle", "circle open", "square open", "triangle open")) +
  scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB

ggsave(height=6, width=8, dpi=600, file="plots/figure_16.pdf")


# Scenario: Group A, 80% coverage, immune escape at 1.5/2.5 years, low TP, all scenarios (fig 29)

df <- covidData_Base %>% 
  filter(
    group == "A" &
      (immuneEscape == "1.50 yr" | immuneEscape == "2.50 yr") &
      tpLevel == "low TP" &
      boostStart == "2.00 yr")

ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n low TP, boosting starts 2.0 yr"

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open")) +
  scale_color_manual(values=c("purple2", "purple2", "darkcyan","darkcyan",  "red3", "red3")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA1 + cetHigherA1

ggsave(height=6, width=8, dpi=600, file="plots/figure_29.pdf")

 # Scenario: Group B, 80% coverage, immune escape at 2 years, high TP, all scenarios (fig 33)
 df <- covidData_Base %>% 
   filter(
     group == "B" &
       (immuneEscape == "1.50 yr" | immuneEscape == "2.50 yr") &
       tpLevel == "high TP" &
       boostStart == "2.00 yr")
 
 ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, boosting starts 2.0 yr"
 
 ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open")) +
   scale_color_manual(values=c("purple2", "purple2", "darkcyan","darkcyan",  "red3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
 
 ggsave(height=6, width=8, dpi=600, file="plots/figure_33.pdf")

# Scenario 9: Group B, 80% coverage, immune escape at 1.5 years, high TP, all scenarios (fig 37)
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

df <- df %>%
  mutate(scenarioBoostStart = replace(scenarioBoostStart, scenarioBoostStart == "6-monthly boosting at 1.75 yr", "6-monthly boosting"))


ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, immune escape 1.5 yr"

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("asterisk", "circle", "square", "triangle", "diamond")) +
  scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB

ggsave(height=6, width=8, dpi=600, file="plots/figure_37.pdf")

# Scenario: Group B, 80% coverage, immune escape at 2.5 years, high TP, all scenarios (fig 38)
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, immune escape 2.5 yr"


df <- df %>%
  mutate(scenarioBoostStart = replace(scenarioBoostStart, scenarioBoostStart == "6-monthly boosting at 1.75 yr", "6-monthly boosting"))

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("asterisk", "circle open", "square open", "triangle open", "diamond open")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
 
 ggsave(height=6, width=8, dpi=600, file="plots/figure_38.pdf")

 # Scenario: Group B, 20% coverage, immune escape at 2.5 years, high risk, high TP (fig 42)
df <- covidData_Base %>% filter(popType=="Younger" & 
                                  (tpLevel=="low TP" | tpLevel=="high TP") &
                                 (scenario=="High-risk boosting" | scenario=="6-monthly boosting"| scenario=="Random vaccination"| scenario=="Pediatric vaccination") &
                                (vaxCoverage=="20.0%"))
df <- df %>%
  mutate(popTP = paste0(scenario, sep = "-", tpLevel))
 
 ggtitle <- "Scenario: younger population, 20% initial vaccination coverage \n high and low TP, immune escape 2.0 yr"
 
 ggplot(df, aes(x=iDaly, y=iCost, shape=popTP, color=popTP)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open")) +
   scale_color_manual(values=c("purple2", "purple2", "darkcyan","darkcyan",  "red3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsC,  linewidth = 0.3, linetype="dashed") + cetLowerC + cetHigherC
 
 ggsave(height=6, width=8, dpi=600, file="plots/figure_42.pdf")
 
  # # Scenario: Group B, 50% coverage, immune escape at 2.5 years, high risk, high TP (fig 41)
df <- covidData_Base %>% filter(popType=="Younger" & 
                                 (tpLevel=="low TP" | tpLevel=="high TP") &
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"| scenario=="Random vaccination"| scenario=="Pediatric vaccination") &
                                   (vaxCoverage=="50.0%"))
df <- df %>%
  mutate(popTP = paste0(scenario, sep = "-", tpLevel))
 
 ggtitle <- "Scenario: younger population, 50% initial vaccination coverage \n high and low TP, immune escape 2.0 yr"
 
 ggplot(df, aes(x=iDaly, y=iCost, shape=popTP, color=popTP)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open")) +
   scale_color_manual(values=c("purple2", "purple2", "darkcyan","darkcyan",  "red3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
 
 ggsave(height=6, width=8, dpi=600, file="plots/figure_41.pdf")

 






# 
# 
# 
# Scenario 7: Group A, 80% coverage, immune escape at 1.5 years, low TP, all scenarios 
df <- covidData_Base %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="low TP" &
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
ggtitle <- "Scenario: older population, 80% coverage, low TP, immune escape 1.5 yr"

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle open", "square open", "triangle open", "diamond open", "asterisk")) +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA1 + cetHigherA1

ggsave(height=6, width=8, dpi=600, file="plots/scenario_07.pdf")


 
 
# Scenario 8: Group A, 80% coverage, immune escape at 2.5 years, low TP, all scenarios 
df <- covidData_Base %>% filter(group=="A" & immuneEscape=="2.50 yr" & tpLevel=="low TP" &
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
ggtitle <- "Scenario: older population, 80% coverage, low TP, immune escape 2.5 yr"

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle open", "square open", "triangle open", "diamond open", "asterisk")) +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA1 + cetHigherA1

ggsave(height=6, width=8, dpi=600, file="plots/scenario_08.pdf")

 
# Scenario 9: Group B, 80% coverage, immune escape at 1.5 years, high TP, all scenarios 
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
ggtitle <- "Scenario: younger population, 80% coverage, high TP, immune escape 1.5 yr, boosting starts 2.0 yr"

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square", "triangle", "diamond", "asterisk")) +
  scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB

ggsave(height=6, width=8, dpi=600, file="plots/scenario_9.pdf")



# Scenario 10: Group B, 80% coverage, immune escape at 2.5 years, high TP, all scenarios 
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))

ggtitle <- "Scenario: younger population, high TP, immune escape 2.5 yr, boosting starts 2.0 yr"
 
ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle open", "square open", "triangle open", "diamond open", "asterisk")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
 
 ggsave(height=6, width=8, dpi=600, file="plots/scenario_10.pdf")
 
 
# # Scenario 11: Group B, 80% coverage, immune escape at 1.5 years, high TP, 6 monthly boosting (refer to scenario 9)
# # Scenario 12: Group B, 80% coverage, immune escape at 2.5 years, high TP, 6 monthly boosting (refer to scenario 10)


# Scenario 13: Group B, 50% coverage, immune escape at 2 years, high TP, all scenarios 
 df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.00 yr" & tpLevel=="high TP" & 
                                   (scenario=="High-risk boosting" | scenario=="6-monthly boosting") &
                                   (vaxCoverage=="50.0%"))
 
 ggtitle <- "Scenario: younger population, high TP, immune escape 2.0 yr"
 
 ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "square", "triangle", "diamond", "asterisk")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
 
 ggsave(height=6, width=8, dpi=600, file="plots/scenario_13.pdf")
 


# Scenario 14: Group B, 50% coverage, immune escape at 2 years, low TP, all scenarios 
 df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.00 yr" & tpLevel=="low TP" & 
                                   (scenario=="High-risk boosting" | scenario=="6-monthly boosting") &
                                   (vaxCoverage=="50.0%"))
 
 ggtitle <- "Scenario: younger population, low TP, immune escape 2.0 yr"
 
 ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle open", "square open", "triangle open", "circle open", "square open", "triangle open")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
 
 ggsave(height=6, width=8, dpi=600, file="plots/scenario_14.pdf")



# # Scenario 15: Group C, 20% coverage, immune escape at 2 years, high TP, boosting from 2 years (refer to scenario 13)
# # Scenario 16: Group C, 20% coverage, immune escape at 2 years, low TP, boosting from 2 years (refer to scenario 14)
# 
# 
# 
# # Scenario: Group B, 80% coverage, immune escape at 1.5 years, high TP, all scenarios
 df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
                                   (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
                      
 
 ggtitle <- "Scenario: younger population, high TP, immune escape 1.5 yr, boosting starts 2.0 yr"
 
 ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "square", "triangle", "diamond", "asterisk")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
 
 ggsave(height=6, width=8, dpi=600, file="plots/scenario_17.pdf")

# 
# # Scenario: Group B, 80% coverage, immune escape at 1.5 years, low TP, all scenarios
 df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="low TP" & 
                                   (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
 
 
 ggtitle <- "Scenario: younger population, low TP, immune escape 1.5 yr, boosting starts 2.0 yr"
 
 ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle open", "square open", "triangle open", "diamond open", "asterisk")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
 
 ggsave(height=6, width=8, dpi=600, file="plots/scenario_17.pdf")
 

