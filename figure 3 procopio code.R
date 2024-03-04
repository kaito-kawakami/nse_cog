###### NSE Cog Paper, Figure 2. Showing Decline in For SCA using data from Procopio et al., 2022)
setwd("C:/Users/Kaito/Desktop/NSE cog/Procopio SCA (Figure 3)")

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

# Load Data
correlations <- read_excel("SCA summary for Kaito.xlsx", 
                           sheet = "Sheet1", col_types = c("numeric", 
                                                           "text", "numeric", "numeric", "numeric", 
                                                           "numeric", "numeric", "numeric", "text"))

# Define custom labels for the x-axis
custom_labels <- c(
  "1" = "0-7",
  "2" = "7-12",
  "3" = "12-18",
  "4" = "18-64",
  "5" = ">65"
)

# Main Figure
p1 <- correlations %>% 
  ggplot() +
  geom_point(aes(x=`Age category`, y=C, color= `Full Name`), shape=16) +
  geom_line(aes(x=`Age category`, y=C, color= `Full Name`), linewidth = 2) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  scale_color_met_d(name="Redon") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5), labels = custom_labels)+
  scale_y_continuous(limits = c(0, 0.65))+
  labs(
    y = "Shared Environmental Component",
    x = "Age Group", color = "Specific Cognitive Ability") +
  #facet_wrap(~ SCA) +
  facet_wrap(~  factor(`Full Name`, levels=c('Specific Cognitive Abilities (mean)', 'Auditory Processing', 'Comprehension-Knowledge', 'Fluid Reasoning',
                                     'General Knowledge', 'Long-term Memory', 
                                     'Quantitative Knowledge', 'Reading and Writing', 
                                     'Processing Speed', 'Short-term Memory', 'Reaction and Decision Speed',
                                     'Visual Processing'))) +
  theme_ipsum_rc(base_size = 18, axis_title_size = 20, strip_text_size = 16, axis_title_just = "c") +
  theme(legend.position = "none")

p1





