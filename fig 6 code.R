setwd("C:/Users/Kaito/Desktop/NSE cog/educational attainment (Figure 6)")
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
library(ggrepel)


# Prep data
data <- read_excel("fig 6 data.xlsx", 
                         col_types = c("text", "numeric", "numeric", 
                                       "text"))

data$Trait <- as.factor(data$Trait)

library(dplyr)
library(ggplot2)

# Scale function (as defined before)
scale_function <- function(x) {
  25 + (x - 25) * (25 / 45)
}

# Inverse scale function to map back to original ages
inverse_scale_function <- function(x) {
  if (x <= 25) {
    return(x)
  } else {
    return(25 + (x - 25) * (45 / 25))
  }
}

# Transform data
transformed_data <- data %>%
  mutate(Age_transformed = ifelse(Age <= 25, Age, scale_function(Age)))

# Define breaks and labels for the x-axis
x_breaks <- c(0, 5, 10, 15, 20, 25, scale_function(45), scale_function(70))
x_labels <- c(0, 5, 10, 15, 20, 25, 45, 70)

highlights <- c(
  "A-Level enrolment", "University enrolment", "Educational attainment",
  "Educational achievement (primary & secondary)", "A-level grades", "University grades"
)

transformed_data <- transformed_data %>%
  mutate(
    print = ifelse(Label %in% highlights, Label, "")
  )

# Plot
ggplot(transformed_data, aes(x = Age_transformed, y = C, color = Trait)) +
  geom_line(aes(linetype = Trait), size = 2) +
  geom_point(aes(fill = Trait), shape = 21, size = 6) +
  scale_linetype_manual(values = c("Educational Attainment" = "dotted", "g" = "solid", "Specific Cognitive Abilities" = "longdash", "School Achievement" = "dotted")) +
  scale_color_manual(values = c("Educational Attainment" = "#009E73", "g" = "#56B4E9", "Specific Cognitive Abilities" = "#ffc40c", "School Achievement" = "#990099")) +
  scale_fill_manual(values = c("Educational Attainment" = "#009E73", "g" = "#0000cd", "Specific Cognitive Abilities" = "#ff4500", "School Achievement" = "#990099")) +
  theme_ipsum_rc(base_size = 25, axis_title_size = 25, strip_text_size = 20, axis_title_just = "c") +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(
    y = "Shared Environmental Component",
    x = "Age (transformed)"
  ) +
  geom_curve(
    aes(x = 5, y = 0.15, xend = 3, yend = 0.29),
    arrow = arrow(
      length = unit(0.03, "npc"), 
      type="open" # Describes arrow head (open or closed)
    ),
    colour = "#0000cd",
    size = 1.2,
    angle = 0 # Anything other than 90 or 0 can look unusual
  ) +
  geom_curve(
    aes(x = 4.6, y = 0.41, xend = 3.5, yend = 0.4),
    arrow = arrow(
      length = unit(0.03, "npc"), 
      type="open" 
    ),
    colour = "#ff4500",
    size = 1.2,
    angle = 0 
  )+
  geom_label(
    label="Specific Cognitive Abilities", 
    x=10,
    y=0.41,
    color = "#ff4500",
    size = 6.3)+
  geom_label(
    label="General Cognitive Ability", 
    x=5.9,
    y=0.14,
    color = "#0000cd",
    size = 6.3)+
  annotate(
    geom = "text", 
    x = 11, 
    y = 0.23, 
    label = "0.1", 
    color = "white",  size = 1
  )+
  geom_label_repel(
    aes(label = print),
    color = "black",
    size = 16/.pt, # font size 9 pt
    point.padding = 0.1, 
    box.padding = 0.6,
    min.segment.length = 0,
    max.overlaps = 1000, # labels for the points
    force = 250
  ) 
  


  



