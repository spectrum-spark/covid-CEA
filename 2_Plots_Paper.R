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
                legend.text = element_text(size=12, family = "DejaVu Sans"),   #change legend text font size
                legend.title = element_text(size=12), #change legend title font size
                legend.key.size = unit(0.5, 'cm'),      #change legend key size
) 

cetLowerA  <- annotate("text", y = 750000, x = 90, size=4, label = "CET = $19,000", family = "DejaVu Sans")
cetHigherA <- annotate("text", y = 680000, x =  -10,  size=4, label = "CET = $30,000", family = "DejaVu Sans")

cetLowerB  <- annotate("text", y = 100000, x = 375, size=4, label = "CET = $200",    family = "DejaVu Sans")
cetHigherB <- annotate("text", y = 650000, x = 375, size=4, label = "CET = $1,600",  family = "DejaVu Sans")

cetLowerC  <- annotate("text", y = 100000, x = 375, size=4, label = "CET = $100",    family = "DejaVu Sans")
cetHigherC <- annotate("text", y = 650000, x = 375, size=4, label = "CET = $1,000",  family = "DejaVu Sans")

## Style guide for points on scatter plots
# filled shapes for 1.5y and plus/open for 2.5y; plus/open shapes for "low TP" and filled for "high TP"
# shapes: circle, square, triangle, diamond; their plus and open counterparts; asterisk, cross, plus


############ Figure 3

# Fig 3a
df <- covidData_Base %>% filter(popType=="Older" & tpLevel=="high TP" & boostStart=="2.00 yr" & 
                                  (immuneEscape=="1.50 yr" | immuneEscape=="2.50 yr"))
#ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n high TP, boosting starts 2.0 yr"
df$scenarioImmuneEscape<- factor(df$scenarioImmuneEscape,levels = c("Pediatric boost, immune esc 1.50 yr","Pediatric boost, immune esc 2.50 yr","High-risk boost, immune esc 1.50 yr","High-risk boost, immune esc 2.50 yr",  "Random boost, immune esc 1.50 yr" ,"Random boost, immune esc 2.50 yr"))
figure3a <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle open", "square", "square open", "triangle", "triangle open"),name="older population") +
  scale_color_manual(values=c("#ff0000","#ff0000", "#fa8072", "#fa8072", "#b22222", "#b22222"),name="older population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme+ theme(legend.position = c(1.05, 0.60)) + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA+
  theme(text=element_text(family="DejaVu Sans"))



# Fig 3b
df <- covidData_Base %>% filter(group == "B" & (immuneEscape == "1.50 yr" | immuneEscape == "2.50 yr") &
                                  tpLevel == "high TP" & boostStart == "2.00 yr")

#ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, boosting starts 2.0 yr"
df$scenarioImmuneEscape<- factor(df$scenarioImmuneEscape,levels = c("Pediatric boost, immune esc 1.50 yr","Pediatric boost, immune esc 2.50 yr","High-risk boost, immune esc 1.50 yr","High-risk boost, immune esc 2.50 yr",  "Random boost, immune esc 1.50 yr" ,"Random boost, immune esc 2.50 yr"))

figure3b<- ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) + 
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle open", "square", "square open","triangle", "triangle open"),name="younger population") +
  scale_color_manual(values=c("#1e90ff", "#1e90ff", "#87cefa","#87cefa",  "#000080", "#000080"),name="younger population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB+
  theme(text=element_text(family="DejaVu Sans")) + theme(legend.position = c(1.05, 0.0))

plot_grid(figure3a, figure3b, labels = c("(a)","(b)"),label_x=0.12,label_y = 0.98)

ggsave(height=5, width=12, dpi=600, file="plots/figure_3.pdf")


############ Figure 5



# Fig 5a
df <- covidData_Base %>% filter(popType=="Older" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boost" | scenario=="6-monthly boost"))
df[df == "6-monthly boost at 1.75 yr"] <- "Half-yearly boosting" # Update 6-monthly name

df$scenarioBoostStart<- factor(df$scenarioBoostStart,levels = c("High-risk boost at 1.75 yr", "High-risk boost at 2.00 yr","High-risk boost at 2.25 yr","High-risk boost at 2.50 yr","Half-yearly boosting"))
# ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n high TP, immune escape 1.5 yr"
figure5a <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle open", "square", "triangle", "diamond","asterisk"), name = "older population (immune escape 1.5 yr)") +
  scale_color_manual(values=c("#fa8072", "#fa8072", "#ff0000", "#b22222","#9370db"), name = "older population (immune escape 1.5 yr)") +
  xlab + ylab + xscale + yscale + hline + vline + border +theme +  # ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA



# Fig 5b
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="1.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boost" | scenario=="6-monthly boost"))
df[df == "6-monthly boost at 1.75 yr"] <- "Half-yearly boosting" # Update 6-monthly name
df$scenarioBoostStart<- factor(df$scenarioBoostStart,levels = c("High-risk boost at 1.75 yr", "High-risk boost at 2.00 yr","High-risk boost at 2.25 yr","High-risk boost at 2.50 yr","Half-yearly boosting"))
#ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, immune escape 1.5 yr"
figure5b <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c( "circle open", "square", "triangle", "diamond","asterisk"), name = "younger population (immune escape 1.5 yr)") +
  scale_color_manual(values=c( "#87cefa", "#87cefa", "#1e90ff", "#000080","#9370db"), name = "younger population (immune escape 1.5 yr)") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #  ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB



# Fig 5c
df <- covidData_Base %>% filter(popType=="Older" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boost" | scenario=="6-monthly boost"))
df[df == "6-monthly boost at 1.75 yr"] <- "Half-yearly boosting" # Update 6-monthly name
df$scenarioBoostStart<- factor(df$scenarioBoostStart,levels = c("High-risk boost at 1.75 yr", "High-risk boost at 2.00 yr","High-risk boost at 2.25 yr","High-risk boost at 2.50 yr","Half-yearly boosting"))
#ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n high TP, immune escape 2.5 yr"
figure5c <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle open", "square", "triangle", "diamond","asterisk") , name = "older population (immune escape 2.5 yr)") +
  scale_color_manual(values=c("#fa8072", "#fa8072", "#ff0000", "#b22222","#9370db"), name = "older population (immune escape 2.5 yr)") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + # ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA + theme(legend.position = c(1.0, 0.60))



# Fig 5d
df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boost" | scenario=="6-monthly boost"))
#ggtitle <- "Scenario: younger population, 80% initial vaccination coverage \n high TP, immune escape 2.5 yr"
df[df == "6-monthly boost at 1.75 yr"] <- "Half-yearly boosting" # Update 6-monthly name
df$scenarioBoostStart<- factor(df$scenarioBoostStart,levels = c("High-risk boost at 1.75 yr", "High-risk boost at 2.00 yr","High-risk boost at 2.25 yr","High-risk boost at 2.50 yr","Half-yearly boosting"))
figure5d <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioBoostStart, color=scenarioBoostStart)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle open", "square", "triangle", "diamond", "asterisk") , name = "younger population (immune escape 2.5 yr)") +
  scale_color_manual(values=c("#87cefa", "#87cefa", "#1e90ff", "#000080","#9370db"), name = "younger population (immune escape 2.5 yr)") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + # ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB


plot_grid(figure5a, figure5b, figure5c, figure5d, labels = c("(a)","(b)","(c)","(d)"),label_x=0.12,label_y = 0.98, ncol = 2)

ggsave(height=10, width=12, dpi=600, file="plots/figure_5.pdf")







###################### Figure 7 c


df <- covidData_Base %>% filter(popType=="Younger" & immuneEscape=="2.00 yr" & tpLevel=="high TP" & 
                                  (scenario=="High-risk boost" | scenario=="Pediatric vax" | 
                                     scenario=="Random vax") & boostStart=="2.00 yr" & 
                                  (vaxCoverage=="20.0%" | vaxCoverage=="50.0%"))
#ggtitle <- "Scenario: younger population, high TP, immune escape 2.0 yr, boosting starts 2.0 yr"




df[df == "Random vax, coverage 20.0%"] <- "New primary vax (low coverage)" # Update  name
df[df == "Random vax, coverage 50.0%"] <- "New primary vax (medium coverage)"
df[df == "Pediatric vax, coverage 20.0%"] <- "Pediatric vax (low coverage)"
df[df == "Pediatric vax, coverage 50.0%"] <- "Pediatric vax (medium coverage)"
df[df == "High-risk boost, coverage 20.0%"] <- "High-risk boost (low coverage)"
df[df == "High-risk boost, coverage 50.0%"] <- "High-risk boost (medium coverage)"


df$scenarioVaxCoverage<- factor(df$scenarioVaxCoverage,levels = c("Pediatric vax (low coverage)","High-risk boost (low coverage)", "New primary vax (low coverage)","Pediatric vax (medium coverage)", "High-risk boost (medium coverage)","New primary vax (medium coverage)"))

ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioVaxCoverage, color=scenarioVaxCoverage)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square", "triangle", "circle open", "square open", "triangle open"), name = "younger population") +
  scale_color_manual(values=c("#87cefa", "#1e90ff", "#000080", "#87cefa", "#1e90ff",  "#000080"), name = "younger population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB
ggsave(height=5, width=6, dpi=600, file="plots/figure_7c.pdf")


############# Figure F5


# Fig F5a
df <- covidData_Base %>% filter(popType=="Younger" & (tpLevel=="low TP" | tpLevel=="high TP") & (vaxCoverage=="20.0%") & 
                                  (scenario=="High-risk boost" | scenario=="6-monthly boost"| scenario=="Random vax"| scenario=="Pediatric vax"))
df <- df %>% mutate(popTP = paste0(scenario, sep = ", ", tpLevel))
df$popTP <- factor(df$popTP,levels=c("Pediatric vax, low TP","High-risk boost, low TP","Random vax, low TP","Pediatric vax, high TP","High-risk boost, high TP","Random vax, high TP"))
df$popTP
#ggtitle <- "Scenario: younger population, 20% initial vaccination coverage \n high and low TP, immune escape 2.0 yr"
figure_F5a <- ggplot(df, aes(x=iDaly, y=iCost, shape=popTP, color=popTP)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square", "triangle","circle open", "square open", "triangle open"),name="younger population") +
  scale_color_manual(values=c("#1e90ff","#87cefa", "#000080",  "#1e90ff", "#87cefa", "#000080"),name="younger population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + # ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsC,  linewidth = 0.3, linetype="dashed") + cetLowerC + cetHigherC


# Fig F5b
df <- covidData_Base %>% filter(popType=="Younger" & 
                                  (tpLevel=="low TP" | tpLevel=="high TP") &
                                  (scenario=="High-risk boost" | scenario=="6-monthly boost"| scenario=="Random vax"| scenario=="Pediatric vax") &
                                  (vaxCoverage=="50.0%"))
df <- df %>% mutate(popTP = paste0(scenario, sep = ", ", tpLevel))
df$popTP <- factor(df$popTP,levels=c("Pediatric vax, low TP","High-risk boost, low TP","Random vax, low TP","Pediatric vax, high TP","High-risk boost, high TP","Random vax, high TP"))
#ggtitle <- "Scenario: younger population, 50% initial vaccination coverage \n high and low TP, immune escape 2.0 yr"
figure_F5b <- ggplot(df, aes(x=iDaly, y=iCost, shape=popTP, color=popTP)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "square", "triangle","circle open", "square open", "triangle open"),name="younger population") +
  scale_color_manual(values=c("#1e90ff","#87cefa", "#000080",  "#1e90ff", "#87cefa", "#000080"),name="younger population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme +# ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB



plot_grid(figure_F5a, figure_F5b, rows=2,labels = c("(a) low vaccination coverage","(b) medium vaccination coverage"),label_x=0.12,label_y = 0.98)
ggsave(height=10, width=8, dpi=600, file="plots/figure_F5.pdf")






########## # old Fig F6c, now F7c 
df <- covidData_Base %>% filter(group == "A" & (immuneEscape == "1.50 yr" | immuneEscape == "2.50 yr") & 
                                  tpLevel == "low TP" & boostStart == "2.00 yr")

#ggtitle <- "Scenario: older population, 80% initial vaccination coverage \n low TP, boosting starts 2.0 yr"
df$scenarioImmuneEscape<- factor(df$scenarioImmuneEscape,levels = c("Pediatric boost, immune esc 1.50 yr","Pediatric boost, immune esc 2.50 yr","High-risk boost, immune esc 1.50 yr","High-risk boost, immune esc 2.50 yr",  "Random boost, immune esc 1.50 yr" ,"Random boost, immune esc 2.50 yr"))
Figure_F7c<-ggplot(df, aes(x=iDaly, y=iCost, shape=scenarioImmuneEscape, color=scenarioImmuneEscape)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle open", "square", "square open", "triangle", "triangle open"),name="older population") +
  scale_color_manual(values=c("#ff0000","#ff0000", "#fa8072", "#fa8072", "#b22222", "#b22222"),name="older population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme +#  ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA

plot_grid(Figure_F7c, labels = c("(c)"),label_x=0.12,label_y = 0.98)
ggsave(height=5, width=8, dpi=600, file="plots/figure_F7c.pdf")




#################################################################################
#################################################################################
#################################################################################


## New ggplot2 theme specifications for all figures below
cetLowerA  <- annotate("text", y = 1500000, x = 120, size=4, label = "CET = $19,000", family = "DejaVu Sans")
cetHigherA <- annotate("text", y = 1400000, x =  0, size=4, label = "CET = $30,000", family = "DejaVu Sans")

cetLowerB  <- annotate("text", y = 700000, x = 375, size=4, label = "CET = $200",    family = "DejaVu Sans")
cetHigherB <- annotate("text", y = 120000, x = 375, size=4, label = "CET = $1,600",  family = "DejaVu Sans")

yscale     <- scale_y_continuous(breaks = seq(-600000, 1500000, 300000), 
                                 limits = c(-600000, 1500000), 
                                 labels = units)

theme <- theme + theme(legend.key.size = unit(0.45, 'cm'))





############ Figure 6 c, d





# Figure 6c: Group A, 80% coverage, immune escape at 2 years, high TP, age scenarios
df <- covidData_Base %>% 
  filter(group=="A" & tpLevel=="high TP" & grepl('65+|55+|45+|35+|25+|16+|5+', scenario))
#df[df == "boost 5+"]   <- "boost 05+"
df$scenario<- factor(df$scenario,levels = c("boost 65+","boost 55+","boost 45+","boost 35+","boost 25+","boost 16+","boost 5+"))
#ggtitle <- "Scenario: older population, 80% coverage, immune escape / boosting 2 yr, high TP"
figure_6c <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenario, color=scenario)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle plus", "square", "square plus", "triangle", "triangle open", "asterisk"),name ="older population") +
  scale_color_manual(values=c("#b22222","#ff0000", "#ffa500", "#ffd700", "#9acd32", "#000080", "#1e90ff"),name ="older population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme +#  ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA




df <- covidData_Base %>% 
  filter(group=="B" & tpLevel=="high TP" & grepl('65+|55+|45+|35+|25+|16+|5+', scenario))
# df[df == "boost 5+"]   <- "boost 05+"
df$scenario<- factor(df$scenario,levels = c("boost 65+","boost 55+","boost 45+","boost 35+","boost 25+","boost 16+","boost 5+"))
#ggtitle <- "Scenario: young population, 80% coverage, immune escape / boosting 2 yr, high TP"
figure_6d <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenario, color=scenario)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle plus", "square", "square plus", 
                              "triangle", "triangle open", "asterisk"),name ="younger population") +
  scale_color_manual(values=c("#b22222","#ff0000", "#ffa500", "#ffd700", "#9acd32", "#000080", "#1e90ff"),name ="younger population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + # ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB  + theme(legend.position = c(0.98, 0.0))



plot_grid(figure_6c, figure_6d, labels = c("(c)","(d)"),label_x=0.12,label_y = 0.98)
ggsave(height=5, width=12, dpi=600, file="plots/figure_6cd.pdf")


################

#Figure F3 


df <- covidData_Base %>% 
  filter(group=="A" & tpLevel=="low TP" & grepl('65+|55+|45+|35+|25+|16+|5+', scenario))
# df[df == "boost 5+"]   <- "boost 05+"
df$scenario<- factor(df$scenario,levels = c("boost 65+","boost 55+","boost 45+","boost 35+","boost 25+","boost 16+","boost 5+"))
# ggtitle <- "Scenario: older population, 80% coverage, immune escape / boosting 2 yr, low TP"
figure_F3a <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenario, color=scenario)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle plus", "square", "square plus", "triangle", "triangle open", "asterisk"),name ="older population") +
  scale_color_manual(values=c("#b22222","#ff0000", "#ffa500", "#ffd700", "#9acd32", "#000080", "#1e90ff"),name ="older population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsA,  linewidth = 0.3, linetype="dashed") + cetLowerA + cetHigherA



df <- covidData_Base %>% 
  filter(group=="B" & tpLevel=="low TP" & grepl('65+|55+|45+|35+|25+|16+|5+', scenario))
# df[df == "boost 5+"]   <- "boost 05+"
df$scenario<- factor(df$scenario,levels = c("boost 65+","boost 55+","boost 45+","boost 35+","boost 25+","boost 16+","boost 5+"))
# ggtitle <- "Scenario: young population, 80% coverage, immune escape / boosting 2 yr, low TP"
figure_F3b <- ggplot(df, aes(x=iDaly, y=iCost, shape=scenario, color=scenario)) +
  geom_point(size=2.5) + labs(shape = "", color = "") +
  scale_shape_manual(values=c("circle", "circle plus", "square", "square plus", 
                              "triangle", "triangle open", "asterisk"),name ="younger population") +
  scale_color_manual(values=c("#b22222","#ff0000", "#ffa500", "#ffd700", "#9acd32", "#000080", "#1e90ff"),name ="younger population") +
  xlab + ylab + xscale + yscale + hline + vline + border + theme + #ggtitle(ggtitle) +
  geom_abline(intercept = 0, slope = cetWoodsB,  linewidth = 0.3, linetype="dashed") + cetLowerB + cetHigherB + theme(legend.position = c(0.98, 0.0))



plot_grid(figure_F3a, figure_F3b, rows=2,labels = c("(a)","(b)"),label_x=0.12,label_y = 0.98)
ggsave(height=10, width=8, dpi=600, file="plots/figure_F3.pdf")

