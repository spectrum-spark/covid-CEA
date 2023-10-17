### Load required packages
library(tidyverse) # data management package
library(ggplot2)   # for plots
library(cowplot)   # for nicer themes in plots
library(showtext)  # for custom fonts in plots

## default settings for CEAC plots
 
cetWoodsA <- c(low=19000, high=30000)
cetLowerA  <- annotate("text", y = 0.25, x = 19000, size=4, label = "CET = $19,000")
cetHigherA <- annotate("text", y = 0.25, x =  30000,  size=4, label = "CET = $30,000")

cetWoodsB <- c(low=200,  high=1600)
cetLowerB  <- annotate("text", y = 0.25, x = 200, size=4, label = "CET = $200")
cetHigherB <- annotate("text", y = 0.25, x = 1600, size=4, label = "CET = $1,600")


####CEAC plots####
df <- read_csv("data/ceac_old.csv")
df <- df %>%
  mutate(strategy = case_when(
    strategy == "High-risk boosting, older population, immune escape starts 2.5yr, boosting at 2.0yr" ~ "High-risk boosting, immune escape starts 2.5yr, boosting at 2.0yr",
    strategy == "Paediatric boosting, older population, immune escape starts 2.5yr, boosting at 2.0yr" ~ "Paediatric boosting, immune escape starts 2.5yr, boosting at 2.0yr",
    TRUE ~ strategy
  ))

figceac1 <- ggplot(df) +
geom_line(aes(WTP, Boost, color = strategy), linewidth=0.5, linetype = "solid") +
  xlab("Cost-effectiveness threshold") + ylab("Probability cost-effective") +
  labs(color = "older population") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 40000, 5000),   limits = c(0, 40000)) +
  theme(axis.title        = element_text(size = 14), 
        axis.text         = element_text(size = 10,  color = "black"),
        axis.line         = element_line(linewidth = 0, color = "black"),
        axis.ticks        = element_line(size = 0.2, color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major  = element_line(size = 0.25, colour = "gray99")) +
  geom_vline(aes(xintercept=cetWoodsA[1]), linetype="dashed", color="black") +
  geom_vline(aes(xintercept=cetWoodsA[2]), linetype="dashed", color="black") +
  cetLowerA + cetHigherA +
  theme(legend.position = c(0.5, 0.75),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(size=8, family = "DejaVu Sans"),
        legend.title = element_text(size=12),
        legend.background = element_rect(fill="transparent")) +
  scale_color_manual(values = c("High-risk boosting, immune escape starts 2.5yr, boosting at 2.0yr" = "#ff0000", 
                                "Paediatric boosting, immune escape starts 2.5yr, boosting at 2.0yr" = "#000080"))


figceac1

##
df <- read_csv("data/ceac_young.csv")
df <- df %>%
  mutate(strategy = case_when(
    strategy == "High-risk boosting, younger population, immune escape starts 1.5yr, boosting at 2.0yr" ~ "High-risk boosting, immune escape starts 1.5yr, boosting at 2.0yr",
    strategy == "Paediatric boosting, younger population, immune escape starts 1.5yr, boosting at 2.0yr" ~ "Paediatric boosting, immune escape starts 1.5yr, boosting at 2.0yr",
    TRUE ~ strategy
  ))

figceac2 <- ggplot(df) +
  geom_line(aes(WTP, Boost, color = strategy), linewidth=0.5, linetype = "solid") +
  xlab("Cost-effectiveness threshold") + ylab("Probability cost-effective") +
  labs(color = "younger population") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 4000, 500),   limits = c(0, 3000)) +
  theme(axis.title        = element_text(size = 14), 
        axis.text         = element_text(size = 10,  color = "black"),
        axis.line         = element_line(linewidth = 0, color = "black"),
        axis.ticks        = element_line(size = 0.2, color = "black"),
        axis.ticks.length = unit(0.2, "cm"),
        panel.grid.major  = element_line(size = 0.25, colour = "gray99")) +
  geom_vline(aes(xintercept=cetWoodsB[1]), linetype="dashed", color="black") +
  geom_vline(aes(xintercept=cetWoodsB[2]), linetype="dashed", color="black") +
  cetLowerB + cetHigherB +
  theme(legend.position = c(0.6, 0.5),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(size=8, family = "DejaVu Sans"),
        legend.title = element_text(size=12),
        legend.background = element_rect(fill="transparent")) +
  scale_color_manual(values = c("High-risk boosting, immune escape starts 1.5yr, boosting at 2.0yr" = "#1e90ff", 
                                "Paediatric boosting, immune escape starts 1.5yr, boosting at 2.0yr" = "#9370db"))


figceac2


plot_grid(figceac1, figceac2, labels = c("(a)","(b)"),label_x=0.12,label_y = 0.98, ncol = 2)

ggsave(height=5, width=12, dpi=600, file="plots/figure_ceac.pdf")

