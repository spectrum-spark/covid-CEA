source("1_Model_Base.R")



### Load required packages
library(ggplot2)   # for plots
library(cowplot)   # for nicer themes in plots
library(showtext)  # for custom fonts in plots
library(scales)    # for plots scale transformations



### default settings for all plots
theme_set(theme_classic(base_family = "univers"))
font_add("univers",   "fonts/UniversRegular.ttf")
showtext_auto()

width      <- 0.75  # width of columns in plot (value between 0 and 1)
ylab       <- ylab("Cost per DALY averted ($)")
xlab       <- xlab("")
xscale     <- scale_x_discrete(name=" ")
border     <- panel_border(color = "#444444", size = 0.3, linetype = 1)
scaleFill  <- scale_fill_manual(values = c("#FEDAB8", "#A162D0"))
scaleColor <- scale_color_manual(values = c("black", "black"))
theme      <- theme(axis.title.x      = element_text(size = 12), 
                    axis.title.y      = element_text(size = 12), 
                    axis.text         = element_text(size = 11,  color = "black"),
                    axis.line         = element_line(linewidth = 0.1, color = "#444444"),
                    axis.ticks        = element_line(linewidth = 0.2, color = "black"),
                    axis.ticks.length = unit(0.1, "cm"),
                    panel.grid.major  = element_line(linewidth = 0.2, colour = "gray97"),
                    plot.title        = element_text(size = 14, colour = "black", 
                                                     margin=margin(t=5, b=5), hjust = 0.5),
                    legend.position = c(1, 0), 
                    legend.justification = c(1.1, 0), 
                    legend.background = element_rect(fill="transparent"),
                    legend.text = element_text(size=10),   # change legend text font size
                    legend.title = element_blank(),        # change legend title font size
                    legend.key.size = unit(0.3, 'cm')      # change legend key size
) 



### Function for rescaling plots to a tornado
offset_trans <- function(offset=0) {
  trans_new(paste0("offset-", format(offset)), function(x) x-offset, function(x) x+offset)
}



### Scenario 7: Group A, 80% coverage, immune escape at 1.5 years, low TP, all scenarios

# Get the base case icer value
baseCase <- covidData_Base %>%  
  filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="low TP" & scenario=="High-risk boosting" & boostStart=="2.00 yr") %>% 
  .$icer
baseCase

# Get the sensitivity analyses results
df <- covidData_OWSA %>%  
  filter(group=="A" & immuneEscape=="1.50 yr" & tpLevel=="low TP" & scenario=="High-risk boosting" & boostStart=="2.00 yr") %>% 
  pivot_wider(names_from = result, values_from = icer) %>% 
  mutate(range = abs(High - Low)) %>% 
  arrange(range) %>% 
  mutate(parameter=factor(x=parameter, levels=parameter)) %>% 
  pivot_longer(names_to='type', values_to='value', Low:High) %>% 
  slice_tail(n=20) # keep only the last 20 observations

# tornado plot
ggplot(df, aes(x=parameter,y=value, fill=type, colour=type)) +
  geom_bar(data=df[df$type=="Low",],  aes(x=parameter,y=value), stat="identity", linewidth=0.1) +
  geom_bar(data=df[df$type=="High",], aes(x=parameter,y=value), stat="identity", linewidth=0.1) +
  geom_hline(yintercept = baseCase, linetype = "solid", size=0.25) +
  theme + xscale + coord_flip() + border + scaleFill + scaleColor +
  scale_y_continuous(name="Incremental cost-effectiveness ratio ($)", trans=offset_trans(offset=baseCase), 
                     limits = c(5000, 19000), breaks=breaks_extended(7)) +
  geom_text(data = subset(df, type == "Low"), show.legend = FALSE, 
            aes(label = comma(round(value))), 
            hjust= ifelse(subset(df, type == "Low", select = value) < baseCase, 1.15, -0.15),
            size = 3, family = "univers") +
  geom_text(data = subset(df, type == "High"), show.legend = FALSE, 
            aes(label = comma(round(value))), 
            hjust= ifelse(subset(df, type == "High", select = value) < baseCase, 1.15, -0.15),
            size = 3, family = "univers") +
  ggtitle("Scenario: older population, 80% coverage, low TP \n immune escape starts 1.5 yr, boosting at 2.0 yr")
ggsave(height=6, width=9, dpi=600, file="plots/tornado_07.pdf")
ggsave(height=6, width=9, dpi=600, file="plots/tornado_07.svg")



### Scenario 7: Group A, 80% coverage, immune escape at 1.5 years, low TP, all scenarios

# Get the base case icer value
baseCase <- covidData_Base %>%  
  filter(group=="B" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & scenario=="High-risk boosting" & boostStart=="2.00 yr") %>% 
  .$icer
baseCase

# Get the sensitivity analyses results
df <- covidData_OWSA %>%  
  filter(group=="B" & immuneEscape=="2.50 yr" & tpLevel=="high TP" & scenario=="High-risk boosting" & boostStart=="2.00 yr") %>% 
  pivot_wider(names_from = result, values_from = icer) %>% 
  mutate(range = abs(High - Low)) %>% 
  arrange(range) %>% 
  mutate(parameter=factor(x=parameter, levels=parameter)) %>% 
  pivot_longer(names_to='type', values_to='value', Low:High) %>% 
  slice_tail(n=20) # keep only the last 20 observations

# tornado plot
ggplot(df, aes(x=parameter,y=value, fill=type, colour=type)) +
  geom_bar(data=df[df$type=="Low",],  aes(x=parameter,y=value), stat="identity", linewidth=0.1) +
  geom_bar(data=df[df$type=="High",], aes(x=parameter,y=value), stat="identity", linewidth=0.1) +
  geom_hline(yintercept = baseCase, linetype = "solid", size=0.25) +
  theme + xscale + coord_flip() + border + scaleFill + scaleColor +
  scale_y_continuous(name="Incremental cost-effectiveness ratio ($)", trans=offset_trans(offset=baseCase), 
                     limits = c(-3000, 1800), breaks=breaks_extended(7)) +
  geom_text(data = subset(df, type == "Low"), show.legend = FALSE, 
            aes(label = comma(round(value))), 
            hjust= ifelse(subset(df, type == "Low", select = value) < baseCase, 1.15, -0.15),
            size = 3, family = "univers") +
  geom_text(data = subset(df, type == "High"), show.legend = FALSE, 
            aes(label = comma(round(value))), 
            hjust= ifelse(subset(df, type == "High", select = value) < baseCase, 1.15, -0.15),
            size = 3, family = "univers") +
  ggtitle("Scenario: older population, 80% coverage, low TP \n immune escape starts 1.5 yr, boosting at 2.0 yr")
ggsave(height=6, width=9, dpi=600, file="plots/tornado_10.pdf")
ggsave(height=6, width=9, dpi=600, file="plots/tornado_10.svg")





