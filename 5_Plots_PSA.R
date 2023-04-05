source("4_Model_PSA.R")

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

# cetLowerC  <- annotate("text", y = 100000, x = 375, size=3, label = "CET: 100",    family = "univers")
# cetHigherC <- annotate("text", y = 650000, x = 375, size=3, label = "CET: 1,000",  family = "univers")



## Style guide for points on scatter plots
# use filled shapes for immune escape at 1.5y and plus/open for 2.5y
# use plus/open shapes for "low TP" and filled shape for "high TP" scenarios
# filled shapes: circle, square, triangle, diamond
# their plus counterparts: circle plus, square plus, triangle square, diamond plus
# their open counterparts: circle open, square open, triangle open, diamond open
# other shapes: asterisk, cross, plus


# Scenario 7: Group A, 80% coverage, immune escape at 1.5 years, low TP, all scenarios 
df_Base <- covidData_Base %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="low TP"&
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))
df_PSA  <- covidData_PSA  %>% filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="low TP"&
                                  (scenario=="High-risk boosting" | scenario=="6-monthly boosting"))



ggtitle <- "Older population, 80% coverage, low TP, immune escape 1.5 yr, boosting starts 2.0 yr"

ggplot() +
  geom_point(data=df_PSA, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, fill="white", shape="circle filled") + 
  geom_point(data=df_Base, aes(x=iDaly, y=iCost, color=scenarioBoostStart), size=2.5, shape="circle") + 
  labs(shape = "", color = "") +
  scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA

ggsave(height=6, width=8, dpi=600, file="plots/PSA_07.pdf")









# ggtitle <- "Scenario: older population, 80% coverage, low TP, immune escape 1.5 yr, boosting starts 2.0 yr"
# 
# ggplot() +
#   geom_point(data=df_PSA, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart), size=2.5, fill="white") + 
#   geom_point(data=df_Base, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart), size=2.5, fill="red") + 
#   labs(shape = "", color = "") +
#   scale_shape_manual(values=c("circle filled", "square filled", "triangle filled", "diamond filled", "asterisk")) +
#   scale_color_manual(values=c("purple2","darkcyan", "red3", "orange3", "gray30")) +
#   xlab + ylab + xscale + yscale + hline + vline + border + theme + ggtitle(ggtitle) +
#   geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA
# 
# ggsave(height=6, width=8, dpi=600, file="plots/PSA_07.pdf")








