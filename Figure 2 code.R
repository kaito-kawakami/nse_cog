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


# extra fig 

theme_update(
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 17, margin = margin(t = 5)),
  axis.text.y = element_text(size = 20, margin = margin(r = 5)),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10", 
    size = 28, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "grey30", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  ),
  # Remove legend
  legend.position = "right"
)

# Plot
plt <- ggplot(
  # The ggplot object has associated the data for the highlighted countries
  falconer_estimates, 
  aes(Age, C, group = Trait)
) + 
  geom_segment(
    data = tibble(y = 0, x1 = 1, x2 = 5),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey60",
    size = .8,
  ) +
  geom_vline(
    aes(xintercept = Age), 
    color = "grey40",
    linetype = "dotted",
    size = .8
  ) +
  coord_cartesian(ylim = c(0, 0.5)) +  # Set y-axis limits 
  geom_line(
    aes(color = Trait,
    size = 2)) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1, 4),
    breaks = seq(1, 4, by = 1),
    labels = custom_labels  # Use custom labels
  )

# Caption
plt <- plt + 
  labs(
    title = "Cognitive Traits show decline in C over time",
    caption = "Estimates of shared environment for Cognitive Traits across four ages (extracted from Polderman et al., 2015)",
    y = "Shared Environmental Variance (C)",
    x = "Age Group", color = "Trait")+
  theme(
    legend.text = element_text(family = "Lato", size = 17, color = "grey40"),
    legend.title = element_text(family = "Lato", size = 17, color = "grey40"),
    axis.title.y = element_text(family = "Lato", size = 17, color = "grey40")  # Setting for y-axis text
  )

# Display the plot
print(plt)

df_labels <- falconer_estimates %>%
  group_by(Trait) %>% na.omit() %>% 
  filter(Age == min(Age, na.rm = TRUE))

df_labels <- df_labels %>% mutate(name_lab = Trait)


# Add labels in the beginning
plt2 <- plt + 
  geom_text_repel(
    data = df_labels,  # Use the filtered data here
    aes(color = Age, label = name_lab),
    family = "Lato",
    fontface = "bold",
    size = 8,
    direction = "y",
    xlim = c(0.5, 0.9)  # Adjust the upper limit here
  ) +
  ## coordinate system + scales
  coord_cartesian(
    clip = "on",
    ylim = c(0, 0.55)
  ) +
  scale_x_continuous(
    expand = c(0, 1),
    limits = c(0, 4)
  ) +
  theme(legend.position = "none",
        plot.title = element_text(vjust = 3))
plt2
