source("1_Model_Base.R")

### Load required packages
library(tidyverse) # data management package
library(ggplot2)   # for plots
library(cowplot)   # for nicer themes in plots
library(showtext)  # for custom fonts in plots


### default settings for all plots
theme_set(theme_classic(base_family = "univers"))
font_add("univers",   "fonts/UniversRegular.ttf")
font_add("universCn",   "fonts/UniversCnRg.ttf")
showtext_auto()


cetWoodsA <- c(low=19000, high=30000)
cetWoodsB <- c(low=200,  high=1600)
cetWoodsC <- c(low=100,  high=1000)

xlab <- xlab("DALYs averted per 100,000 pop")
ylab <- ylab("Incremental costs ($) per 100,000 pop")

units <- function(n) {
  labels <- ifelse(n < -1e9, paste0(round(n/1e6), 'M'),  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          paste0(round(n/1e6, 1), 'M')  # in millions
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
theme  <- theme(axis.title        = element_text(size = 13), 
                axis.text         = element_text(size = 12,  color = "black"),
                axis.line         = element_line(linewidth = 0.01, color = "#444444"),
                axis.ticks        = element_line(linewidth = 0.3, color = "black"),
                axis.ticks.length = unit(0.1, "cm"),
                panel.grid.major  = element_line(linewidth = 0.3, colour = "gray97"),
                plot.title        = element_text(size = 14, colour = "black", 
                                                 margin=margin(t=5, b=5), hjust = 0.1),
                legend.position = c(1, 0), 
                legend.justification = c(1.02, 0), 
                legend.margin = margin(0, 0, 0, 0),
                #legend.spacing.x = unit(0, 'cm'),
                legend.spacing.y = unit(0, 'cm'),
                legend.background = element_rect(fill="transparent"),
                legend.text = element_text(size=12, family = "universCn"),   #change legend text font size
                legend.title = element_text(size=12), #change legend title font size
                legend.key.size = unit(0.5, 'cm'),      #change legend key size
) 

cetLowerA  <- annotate("text", y = 750000, x = 63, size=4, label = "CET = $19,000", family = "universCn")
cetHigherA <- annotate("text", y = 700000, x =  3,  size=4, label = "CET = $30,000", family = "universCn")

cetLowerB  <- annotate("text", y = 100000, x = 375, size=4, label = "CET = $200",    family = "universCn")
cetHigherB <- annotate("text", y = 650000, x = 375, size=4, label = "CET = $1,600",  family = "universCn")

cetLowerC  <- annotate("text", y = 100000, x = 375, size=4, label = "CET = $100",    family = "universCn")
cetHigherC <- annotate("text", y = 650000, x = 375, size=4, label = "CET = $1,000",  family = "universCn")

## Style guide for points on scatter plots
# filled shapes for 1.5y and plus/open for 2.5y; plus/open shapes for "low TP" and filled for "high TP"
# shapes: circle, square, triangle, diamond; their plus and open counterparts; asterisk, cross, plus




# Fig 3a
df <- covidData_Base %>% filter(popType=="Older" & tpLevel=="high TP" & boostStart=="2.00 yr" & 
                                  (immuneEscape=="1.50 yr" | immuneEscape=="2.50 yr"))
ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n high TP, boosting starts 2.0 yr"
ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle open", "square", "square open", "triangle", "triangle open")) +
  scale_color_manual(values=c("purple2","purple2", "darkcyan", "darkcyan", "red3", "red3")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA
ggsave(height=6, width=8, dpi=600, file="plots/figure_3a.pdf")


# Fig 3b
df <- covidData_Base %>% filter(group == "B" & (immuneEscape == "1.50 yr" | immuneEscape == "2.50 yr") &
                                   tpLevel == "high TP" & boostStart == "2.00 yr")
ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, boosting starts 2.0 yr"
ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) + 
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open")) +
   scale_color_manual(values=c("purple2", "purple2", "darkcyan","darkcyan",  "red3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
ggsave(height=6, width=8, dpi=600, file="plots/figure_3b.pdf")




# Fig 5a
df <- covidData_Base %>% filter(popType=="Older" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boost" | scenario=="6-monthly boost"))
df[df == "6-monthly boost at 1.75 yr"] <- "Half-yearly boost at 1.75 yr" # Update 6-monthly name
ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n high TP, immune escape 1.5 yr"
ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("asterisk","circle", "square", "triangle", "diamond")) +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border +theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA
ggsave(height=6, width=8, dpi=600, file="plots/figure_5a.pdf")


# Fig 5b
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
                                   (scenario=="High-risk boost" | scenario=="6-monthly boost"))
df[df == "6-monthly boost at 1.75 yr"] <- "Half-yearly boost at 1.75 yr" # Update 6-monthly name
ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, immune escape 1.5 yr"
ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("asterisk", "circle", "square", "triangle", "diamond")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
ggsave(height=6, width=8, dpi=600, file="plots/figure_5b.pdf")


# Fig 5c
df <- covidData_Base %>% filter(popType=="Older" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & 
                                   (scenario=="High-risk boost" | scenario=="6-monthly boost"))
df[df == "6-monthly boost at 1.75 yr"] <- "Half-yearly boost at 1.75 yr" # Update 6-monthly name
ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n high TP, immune escape 2.5 yr"
ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("asterisk","circle open", "square open", "triangle open", "diamond open")) +
   scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA
ggsave(height=6, width=8, dpi=600, file="plots/figure_5c.pdf")


# Fig 5d
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & 
                                   (scenario=="High-risk boost" | scenario=="6-monthly boost"))
ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, immune escape 2.5 yr"
df[df == "6-monthly boost at 1.75 yr"] <- "Half-yearly boost at 1.75 yr" # Update 6-monthly name
ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("asterisk", "circle open", "square open", "triangle open", "diamond open")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
ggsave(height=6, width=8, dpi=600, file="plots/figure_5d.pdf")




# Fig 7c
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.00 yr" & tpLevel=="high TP" & 
                                   (scenario=="High-risk boost" | scenario=="Pediatric vax" | 
                                       scenario=="Random vax") & boostStart=="2.00 yr" & 
                                   (vaxCoverage=="20.0%" | vaxCoverage=="50.0%"))
ggtitle <- "Scenario: younger population, high TP, immune escape 2.0 yr, boosting starts 2.0 yr"
ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioVaxCoverage, color=scenarioVaxCoverage)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "square", "triangle", "circle open", "square open", "triangle open")) +
   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
ggsave(height=6, width=8, dpi=600, file="plots/figure_7c.pdf")




# Fig F4a
df <- covidData_Base %>% filter(popType=="Younger" & (tpLevel=="low TP" | tpLevel=="high TP") & (vaxCoverage=="20.0%") & 
                                   (scenario=="High-risk boost" | scenario=="6-monthly boost"| scenario=="Random vax"| scenario=="Pediatric vax"))
df <- df %>% mutate(popTP = paste0(scenario, sep = ", ", tpLevel))
ggtitle <- "Scenario: younger population, 20% initial vaccination coverage \n high and low TP, immune escape 2.0 yr"
ggplot(df, aes(x=iDaly, y=iCost, shape=popTP, color=popTP)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open")) +
   scale_color_manual(values=c("purple2", "purple2", "darkcyan","darkcyan",  "red3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsC,  linewidth = 0.3, linetype="dashed") + cetLowerC + cetHigherC
ggsave(height=6, width=8, dpi=600, file="plots/figure_F4a.pdf")


# Fig F4b
df <- covidData_Base %>% filter(popType=="Younger" & 
                                   (tpLevel=="low TP" | tpLevel=="high TP") &
                                   (scenario=="High-risk boost" | scenario=="6-monthly boost"| scenario=="Random vax"| scenario=="Pediatric vax") &
                                   (vaxCoverage=="50.0%"))
df <- df %>% mutate(popTP = paste0(scenario, sep = "-", tpLevel))
ggtitle <- "Scenario: younger population, 50% initial vaccination coverage \n high and low TP, immune escape 2.0 yr"
ggplot(df, aes(x=iDaly, y=iCost, shape=popTP, color=popTP)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open")) +
   scale_color_manual(values=c("purple2", "purple2", "darkcyan","darkcyan",  "red3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
ggsave(height=6, width=8, dpi=600, file="plots/figure_F4b.pdf")




# Fig F6c
df <- covidData_Base %>% filter(group == "A" & (immuneEscape == "1.50 yr" | immuneEscape == "2.50 yr") & 
                                   tpLevel == "low TP" & boostStart == "2.00 yr")

ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n low TP, boosting starts 2.0 yr"

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open")) +
   scale_color_manual(values=c("purple2", "purple2", "darkcyan","darkcyan",  "red3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA

ggsave(height=6, width=8, dpi=600, file="plots/figure_F6c.pdf")




# # UNUSED PLOTS
# 
# # Older, 80% coverage, immune escape at 1.5 years, low TP, all scenarios 
# df <- covidData_Base %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="low TP" &
#                                   (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
# ggtitle <- "Scenario: older population, 80% coverage, low TP, immune escape 1.5 yr"
# ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
#   geom_point(size=2.5) + labs(shape = "", color = "") +
#   scale_shape_manual(values=c("circle open", "square open", "triangle open", "diamond open", "asterisk")) +
#   scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
#   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#   geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA
# ggsave(height=6, width=8, dpi=600, file="plots/scenario_07.pdf")
# 
# 
# # Older, 80% coverage, immune escape at 2.5 years, low TP, all scenarios 
# df <- covidData_Base %>% filter(group=="A" & immuneEscape=="2.50 yr" & tpLevel=="low TP" &
#                                   (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
# ggtitle <- "Scenario: older population, 80% coverage, low TP, immune escape 2.5 yr"
# ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
#   geom_point(size=2.5) + labs(shape = "", color = "") +
#   scale_shape_manual(values=c("circle open", "square open", "triangle open", "diamond open", "asterisk")) +
#   scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
#   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#   geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA
# ggsave(height=6, width=8, dpi=600, file="plots/scenario_08.pdf")
# 
# 
# # Younger, 80% coverage, immune escape at 1.5 years, high TP, all scenarios 
# df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
#                                   (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
# ggtitle <- "Scenario: younger population, 80% coverage, high TP, immune escape 1.5 yr, boosting starts 2.0 yr"
# ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
#   geom_point(size=2.5) + labs(shape = "", color = "") +
#   scale_shape_manual(values=c("circle", "square", "triangle", "diamond", "asterisk")) +
#   scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
#   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
# ggsave(height=6, width=8, dpi=600, file="plots/scenario_9.pdf")
# 
# 
# # Younger, 80% coverage, immune escape at 2.5 years, high TP, all scenarios 
# df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & 
#                                   (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
# ggtitle <- "Scenario: younger population, high TP, immune escape 2.5 yr, boosting starts 2.0 yr"
# ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
#    geom_point(size=2.5) + labs(shape = "", color = "") +
#    scale_shape_manual(values=c("circle open", "square open", "triangle open", "diamond open", "asterisk")) +
#    scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
#    xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#    geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
# ggsave(height=6, width=8, dpi=600, file="plots/scenario_10.pdf")
# 
# 
# # Younger, 50% coverage, immune escape at 2 years, high TP, all scenarios 
# df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.00 yr" & tpLevel=="high TP" & 
#                                    (scenario=="High-risk boosting" | scenario=="6-monthly boosting") &
#                                    (vaxCoverage=="50.0%"))
# ggtitle <- "Scenario: younger population, high TP, immune escape 2.0 yr"
# ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
#    geom_point(size=2.5) + labs(shape = "", color = "") +
#    scale_shape_manual(values=c("circle", "square", "triangle", "diamond", "asterisk")) +
#    scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
#    xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#    geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
# ggsave(height=6, width=8, dpi=600, file="plots/scenario_13.pdf")
#  
# 
# # Younger, 50% coverage, immune escape at 2 years, low TP, all scenarios 
# df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.00 yr" & tpLevel=="low TP" & 
#                                    (scenario=="High-risk boosting" | scenario=="6-monthly boosting") &
#                                    (vaxCoverage=="50.0%"))
# ggtitle <- "Scenario: younger population, low TP, immune escape 2.0 yr"
# ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
#    geom_point(size=2.5) + labs(shape = "", color = "") +
#    scale_shape_manual(values=c("circle open", "square open", "triangle open", "circle open", "square open", "triangle open")) +
#    scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
#    xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#    geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
# ggsave(height=6, width=8, dpi=600, file="plots/scenario_14.pdf")
# 
# 
# # Younger, 50% coverage, immune escape at 1.5 years, high TP, all scenarios 
# df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
#                                    (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
# ggtitle <- "Scenario: younger population, high TP, immune escape 1.5 yr, boosting starts 2.0 yr"
# ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
#    geom_point(size=2.5) + labs(shape = "", color = "") +
#    scale_shape_manual(values=c("circle", "square", "triangle", "diamond", "asterisk")) +
#    scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
#    xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#    geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
# ggsave(height=6, width=8, dpi=600, file="plots/scenario_17.pdf")
# 
# 
# # Younger, 50% coverage, immune escape at 1.5 years, low TP, all scenarios 
# df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="low TP" & 
#                                    (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
# ggtitle <- "Scenario: younger population, low TP, immune escape 1.5 yr, boosting starts 2.0 yr"
# 
# ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
#    geom_point(size=2.5) + labs(shape = "", color = "") +
#    scale_shape_manual(values=c("circle open", "square open", "triangle open", "diamond open", "asterisk")) +
#    scale_color_manual(values=c("purple2", "darkcyan", "red3", "purple2", "darkcyan",  "red3")) +
#    xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#    geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
# ggsave(height=6, width=8, dpi=600, file="plots/scenario_17.pdf")

 

 
## New ggplot2 theme specifications for all figures below
cetLowerA  <- annotate("text", y = 1500000, x = 100, size=4, label = "CET = $19,000", family = "universCn")
cetHigherA <- annotate("text", y = 1500000, x =  30, size=4, label = "CET = $30,000", family = "universCn")
 
cetLowerB  <- annotate("text", y = 700000, x = 375, size=4, label = "CET = $200",    family = "universCn")
cetHigherB <- annotate("text", y = 120000, x = 375, size=4, label = "CET = $1,600",  family = "universCn")
 
yscale     <- scale_y_continuous(breaks = seq(-600000, 1500000, 300000), 
                                  limits = c(-600000, 1500000), 
                                  labels = units)
 
theme <- theme + theme(legend.key.size = unit(0.45, 'cm'))


# Figure 6a: Group A, 80% coverage, immune escape at 2 years, high TP, age scenarios
df <- covidData_Base %>% 
   filter(group=="A" & tpLevel=="high TP" & grepl('65+|55+|45+|35+|25+|16+|5+', scenario))
df[df == "boost 5+"]   <- "boost 05+"
ggtitle <- "Scenario: older population, 80% coverage, immune escape / boosting 2 yr, high TP"
figure_6a <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenario, color=scenario)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle plus", "square", "square plus", "triangle", "triangle open", "asterisk")) +
   scale_color_manual(values=c("purple2","purple2", "darkcyan", "darkcyan", "orange3", "orange3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA
ggsave(height=6, width=8, dpi=600, file="plots/figure_6a.pdf")
 

# Figure 6b: Group A, 80% coverage, immune escape at 2 years, low TP, age scenarios 
df <- covidData_Base %>% 
   filter(group=="A" & tpLevel=="low TP" & grepl('65+|55+|45+|35+|25+|16+|5+', scenario))
df[df == "boost 5+"]   <- "boost 05+"
ggtitle <- "Scenario: older population, 80% coverage, immune escape / boosting 2 yr, low TP"
figure_6b <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenario, color=scenario)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle plus", "square", "square plus", "triangle", "triangle open", "asterisk")) +
   scale_color_manual(values=c("purple2","purple2", "darkcyan", "darkcyan", "orange3", "orange3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA
ggsave(height=6, width=8, dpi=600, file="plots/figure_6b.pdf")


# Figure 6 = 6a + 6b
plot_grid(figure_6a, figure_6b, rows = 2)
ggsave(height=10, width=8, dpi=600, file="plots/figure_6.pdf")
 


# OLD Figure F2 (figure labelings and arrangements have changed)
# Figure F2a: Group B, 80% coverage, immune escape at 2 years, high TP, age scenarios
df <- covidData_Base %>% 
   filter(group=="B" & tpLevel=="high TP" & grepl('65+|55+|45+|35+|25+|16+|5+', scenario))
df[df == "boost 5+"]   <- "boost 05+"
ggtitle <- "Scenario: young population, 80% coverage, immune escape / boosting 2 yr, high TP"
figure_F2a <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenario, color=scenario)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle plus", "square", "square plus", 
                               "triangle", "triangle open", "asterisk")) +
   scale_color_manual(values=c("purple2","purple2", "darkcyan", "darkcyan", "orange3", "orange3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
ggsave(height=6, width=8, dpi=600, file="plots/figure_F2a.pdf")


# Figure F2b: Group B, 80% coverage, immune escape at 2 years, low TP, age scenarios 
df <- covidData_Base %>% 
   filter(group=="B" & tpLevel=="low TP" & grepl('65+|55+|45+|35+|25+|16+|5+', scenario))
df[df == "boost 5+"]   <- "boost 05+"
ggtitle <- "Scenario: young population, 80% coverage, immune escape / boosting 2 yr, low TP"
figure_F2b <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenario, color=scenario)) +
   geom_point(size=2.5) + labs(shape = "", color = "") +
   scale_shape_manual(values=c("circle", "circle plus", "square", "square plus", 
                               "triangle", "triangle open", "asterisk")) +
   scale_color_manual(values=c("purple2","purple2", "darkcyan", "darkcyan", "orange3", "orange3", "red3")) +
   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
   geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
ggsave(height=6, width=8, dpi=600, file="plots/figure_F2b.pdf")
 

# Figure F2 = F2a + F2b
plot_grid(figure_F2a, figure_F2b, rows = 2)
ggsave(height=10, width=8, dpi=600, file="plots/figure_F2.pdf")



# Figure home cost = 0
df <- covidData_Base %>% filter(group == "B" &  (scenario=="High-risk boost") &
                                  (boostStart == "2.00 yr") & (vaxCoverage=="80%") & (tpLevel == "high TP") &
                                  (immuneEscape=="1.50 yr" | immuneEscape=="1.75 yr"| immuneEscape=="2.00 yr"| immuneEscape=="2.25 yr")) 


#ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, boosting starts 2.0 yr"
df$scenarioImmuneEscape<- factor(df$scenarioImmuneEscape,levels = c("High-risk boost, immune esc 1.50 yr","High-risk boost, immune esc 1.75 yr", "High-risk boost, immune esc 2.00 yr","High-risk boost, immune esc 2.25 yr"))

fighome1 <- ggplot(df, aes(x=iDaly, y=iCostnoHome, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) + 
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square","triangle", "diamond"),name="younger population (high TP)") +
  scale_color_manual(values=c("#87cefa", "#000080","#1e90ff", "#9370db") ,name="younger population (high TP)") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB+
  theme(text=element_text(family="DejaVu Sans")) + theme(legend.position = c(1.05, 0.0))
#
df <- covidData_Base %>% filter(group == "B" &  (scenario=="High-risk boost") &
                                  (boostStart == "2.00 yr") & (vaxCoverage=="80%") & (tpLevel == "low TP") &
                                  (immuneEscape=="1.50 yr" | immuneEscape=="1.75 yr"| immuneEscape=="2.00 yr"| immuneEscape=="2.25 yr")) 


#ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, boosting starts 2.0 yr"
df$scenarioImmuneEscape<- factor(df$scenarioImmuneEscape,levels = c("High-risk boost, immune esc 1.50 yr","High-risk boost, immune esc 1.75 yr", "High-risk boost, immune esc 2.00 yr","High-risk boost, immune esc 2.25 yr"))

fighome2 <- ggplot(df, aes(x=iDaly, y=iCostnoHome, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) + 
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle open", "square open","triangle open", "diamond open"),name="younger population (low TP)") +
  scale_color_manual(values=c("#87cefa", "#000080","#1e90ff", "#9370db") ,name="younger population (low TP)") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB+
  theme(text=element_text(family="DejaVu Sans")) + theme(legend.position = c(1.05, 0.0))

plot_grid(fighome1, fighome2, labels = c("(a)","(b)"),label_x=0.12,label_y = 0.98)

ggsave(height=5, width=12, dpi=600, file="plots/figure_home0.pdf")


#################################################################################
#################################################################################

## New ggplot2 theme specifications for all figures below
cetLowerA  <- annotate("text", y = 1500000, x = 120, size=4, label = "CET = $19,000", family = "DejaVu Sans")
cetHigherA <- annotate("text", y = 1400000, x =  0, size=4, label = "CET = $30,000", family = "DejaVu Sans")

# cetLowerB  <- annotate("text", y = 700000, x = 375, size=4, label = "CET = $200",    family = "DejaVu Sans")
# cetHigherB <- annotate("text", y = 120000, x = 375, size=4, label = "CET = $1,600",  family = "DejaVu Sans")

cetLowerB  <- annotate("text", y = 700000, x = 375, size=4, label ="CET = $1,600",    family = "DejaVu Sans")
cetHigherB <- annotate("text", y = 120000, x = 375, size=4, label =  "CET = $200",  family = "DejaVu Sans")

# cetLowerB  <- annotate("text", y = 100000, x = 375, size=4, label = "CET = $200",    family = "DejaVu Sans")
# cetHigherB <- annotate("text", y = 650000, x = 375, size=4, label = "CET = $1,600",  family = "DejaVu Sans")


yscale     <- scale_y_continuous(breaks = seq(-600000, 1500000, 300000), 
                                 limits = c(-600000, 1500000), 
                                 labels = units)

theme <- theme + theme(legend.key.size = unit(0.45, 'cm'))

### fig compare scenarios #####
#####80% coverage high-risk boost####
df <- covidData_Base %>% filter(group == "B" &  (scenario=="High-risk boost") &
                                  (boostStart == "2.00 yr") & (vaxCoverage=="80%") & (tpLevel == "high TP") &
                                  (immuneEscape=="1.50 yr"| immuneEscape=="2.50 yr")) 
df <- df %>%
  pivot_longer(
    cols = c(iCostDonated, iCost, iCostnoHome, iCostnoHomeDonated),
    names_to = "CostType",
    values_to = "CostValue"
  )

df <- df %>%
  mutate(CostType = case_when(
    CostType == "iCostDonated" ~ "Vaccine donated scenario",
    CostType == "iCost" ~ "Base scenario",
    CostType == "iCostnoHome" ~ "No home care cost scenario",
    CostType == "iCostnoHomeDonated" ~ "Vaccine donated and no home care cost scenario",
    TRUE ~ CostType
  ))

df <- df %>%
  mutate(
    CostType = case_when(
      immuneEscape == "1.50 yr" ~ paste(CostType, "1.5 yr"),
      immuneEscape == "2.50 yr" ~ paste(CostType, "2.5 yr"),
      TRUE ~ CostType
    )
  )

df$CostType<- factor(df$CostType,levels = c("Base scenario 1.5 yr",
                                            "Vaccine donated scenario 1.5 yr",
                                            "No home care cost scenario 1.5 yr",
                                            "Vaccine donated and no home care cost scenario 1.5 yr",
                                            "Base scenario 2.5 yr",
                                            "Vaccine donated scenario 2.5 yr",
                                            "No home care cost scenario 2.5 yr",
                                            "Vaccine donated and no home care cost scenario 2.5 yr"
))

###plot
figscenario1 <- ggplot(df, aes(x=iDaly, y=CostValue, shape=CostType, color=CostType)) + 
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square","triangle", "diamond", "circle open", "square open","triangle open", "diamond open"),name="younger population high-risk boosting (80% coverage, high TP)") +
  scale_color_manual(values=c("#87cefa", "#000080","#1e90ff", "#9370db","#87cefa", "#000080","#1e90ff", "#9370db") ,name="younger population high-risk boosting (80% coverage, high TP)") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB+
  theme(text=element_text(family="DejaVu Sans")) + theme(legend.position = c(0.86, 0.6)) +
  theme(legend.text = element_text(size=12, family = "universCn"),
        legend.title = element_text(size=12), 
        legend.key.size = unit(0.5, 'cm')) 


figscenario1
##
df <- covidData_Base %>% filter(group == "B" &  (scenario=="High-risk boost") &
                                  (boostStart == "2.00 yr") & (vaxCoverage=="80%") & (tpLevel == "low TP") &
                                  (immuneEscape=="1.50 yr"| immuneEscape=="2.50 yr")) 
df <- df %>%
  pivot_longer(
    cols = c(iCostDonated, iCost, iCostnoHome, iCostnoHomeDonated),
    names_to = "CostType",
    values_to = "CostValue"
  )

df <- df %>%
  mutate(CostType = case_when(
    CostType == "iCostDonated" ~ "Vaccine donated scenario",
    CostType == "iCost" ~ "Base scenario",
    CostType == "iCostnoHome" ~ "No home care cost scenario",
    CostType == "iCostnoHomeDonated" ~ "Vaccine donated and no home care cost scenario",
    TRUE ~ CostType
  ))

df <- df %>%
  mutate(
    CostType = case_when(
      immuneEscape == "1.50 yr" ~ paste(CostType, "1.5 yr"),
      immuneEscape == "2.50 yr" ~ paste(CostType, "2.5 yr"),
      TRUE ~ CostType
    )
  )

df$CostType<- factor(df$CostType,levels = c("Base scenario 1.5 yr",
                                            "Vaccine donated scenario 1.5 yr",
                                            "No home care cost scenario 1.5 yr",
                                            "Vaccine donated and no home care cost scenario 1.5 yr",
                                            "Base scenario 2.5 yr",
                                            "Vaccine donated scenario 2.5 yr",
                                            "No home care cost scenario 2.5 yr",
                                            "Vaccine donated and no home care cost scenario 2.5 yr"
))

###plot
figscenario2 <- ggplot(df, aes(x=iDaly, y=CostValue, shape=CostType, color=CostType)) + 
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square","triangle", "diamond", "circle open", "square open","triangle open", "diamond open"),name="younger population high-risk boosting (80% coverage, low TP)") +
  scale_color_manual(values=c("#87cefa", "#000080","#1e90ff", "#9370db","#87cefa", "#000080","#1e90ff", "#9370db") ,name="younger population high-risk boosting (80% coverage, low TP)") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB+
  theme(text=element_text(family="DejaVu Sans")) + theme(legend.position = c(0.85, 0.6)) +
  theme(legend.text = element_text(size=12, family = "universCn"),
        legend.title = element_text(size=12), 
        legend.key.size = unit(0.5, 'cm')) 

figscenario2

plot_grid(figscenario1, figscenario2, labels = c("(a)","(b)"),label_x=0.12,label_y = 0.98, nrow = 2)

ggsave(height=10, width=8, dpi=600, file="plots/figure_scenarioCompare1.pdf")

######20% coverage boost####
df <- covidData_Base %>% filter(group == "C" & (boostStart == "2.00 yr")& (vaxCoverage=="20.0%") &
                                  scenario == "High-risk boost")

df <- df %>%
  pivot_longer(
    cols = c(iCostDonated, iCost, iCostnoHome, iCostnoHomeDonated),
    names_to = "CostType",
    values_to = "CostValue"
  )

df <- df %>%
  mutate(CostType = case_when(
    CostType == "iCostDonated" ~ "Vaccine donated scenario",
    CostType == "iCost" ~ "Base scenario",
    CostType == "iCostnoHome" ~ "No home care cost scenario",
    CostType == "iCostnoHomeDonated" ~ "Vaccine donated and no home care cost scenario",
    TRUE ~ CostType
  ))

df <- df %>%
  mutate(
    CostType = case_when(
      tpLevel == "high TP" ~ paste(CostType, "high TP"),
      tpLevel == "low TP" ~ paste(CostType, "low TP"),
      TRUE ~ CostType
    )
  )

df$CostType<- factor(df$CostType,levels = c("Base scenario high TP",
                                            "Vaccine donated scenario high TP",
                                            "No home care cost scenario high TP",
                                            "Vaccine donated and no home care cost scenario high TP",
                                            "Base scenario low TP",
                                            "Vaccine donated scenario low TP",
                                            "No home care cost scenario low TP",
                                            "Vaccine donated and no home care cost scenario low TP"
))


figscenario3 <- ggplot(df, aes(x=iDaly, y=CostValue, shape=CostType, color=CostType)) + 
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square","triangle", "diamond", "circle open", "square open","triangle open", "diamond open"),name="younger population high-risk boosting (20% coverage)") +
  scale_color_manual(values=c("#87cefa", "#000080","#1e90ff", "#9370db","#87cefa", "#000080","#1e90ff", "#9370db") ,name="younger population high-risk boosting (20% coverage)") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB+
  theme(text=element_text(family="DejaVu Sans")) + theme(legend.position = c(0.73, 0.68)) +
  theme(legend.text = element_text(size=12, family = "universCn"),
        legend.title = element_text(size=12), 
        legend.key.size = unit(0.5, 'cm')) 


figscenario3

ggsave(height=6, width=8, dpi=600, file="plots/figure_scenarioCompare2.pdf")

