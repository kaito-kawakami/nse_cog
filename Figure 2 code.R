###### NSE Cog Paper, Figure 1. Showing Decline in Cog traits using data from Polderman et al., 2015)
setwd("C:/Users/Kaito/Desktop/NSE cog/Polderman SCA (Figure 2)")

library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)
library(readxl)
library(hrbrthemes)

# Polderman Time series 
falconer_estimates <- read_excel("falconer_estimates.xlsx")

# remove all ages
falconer_estimates <- falconer_estimates %>% filter(!Age == "All Ages")

#change cols to numbers
falconer_estimates$Age <- as.numeric(falconer_estimates$Age)
falconer_estimates$C <- as.numeric(falconer_estimates$C)
falconer_estimates$Trait <- as.factor(falconer_estimates$Trait)

#graph 
# Define custom labels for the x-axis
custom_labels <- c(
  "1" = "0-11",
  "2" = "12-17",
  "3" = "18-64",
  "4" = "65+"
)

#### MISC ####
falconer_estimates <- as.data.frame(falconer_estimates)

falconer_estimates <- falconer_estimates %>% filter(Trait != "General cognitive ability")
falconer_estimates$Trait <- factor(falconer_estimates$Trait, 
                                   levels = c("Verbal Ability", 
                                              "Memory", 
                                              "Numerical Ability"))

# Main Figure
p1 <- falconer_estimates %>%
  ggplot(aes(x=Age, y=C, color= Trait)) +
  geom_point(data=falconer_estimates %>% group_by(Trait), shape=16) +
  geom_line(aes(x= Age, y= C, color= Trait), linewidth = 2) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  scale_color_met_d(name="Redon") +
  scale_x_continuous(breaks = c(1, 2, 3, 4), labels = custom_labels)+
  scale_y_continuous(limits = c(0, 0.3)) +
  labs(
    y = "Shared Environmental Component",
    x = "Age Group", color = "Trait") +
  facet_wrap(~ Trait) +
  coord_cartesian(clip = "off") +
  theme_ipsum_rc(base_size = 25, axis_title_size = 25, strip_text_size = 20, axis_title_just = "c")+
  theme(legend.position = "none")

p1 

