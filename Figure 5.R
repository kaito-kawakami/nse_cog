setwd("C:/Users/Kaito/Desktop/NSE cog/Educational Achievement (Figure 5)")

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

# import and format data
fig_5 <- read_excel("fig 5 data.xlsx")
fig_5$Trait <- as.factor(fig_5$Trait)
fig_5$Category <- as.factor(fig_5$Category)

# Reduce the number of 'empty bars' to add at the end of each group for smaller spaces
empty_bar <- 1
to_add <- data.frame(matrix(NA, empty_bar * nlevels(fig_5$Category), ncol(fig_5)))
colnames(to_add) <- colnames(fig_5)
to_add$Category <- rep(levels(fig_5$Category), each = empty_bar)
fig_5 <- rbind(fig_5, to_add)
fig_5 <- fig_5 %>% arrange(Category)
fig_5$id <- seq(1, nrow(fig_5))

# Get the name and the y position of each label
label_data <- fig_5
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- fig_5 %>% 
  group_by(Category) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


# Make the plot
plt <- ggplot(fig_5, aes(x = as.factor(id), y = C_Estimate, fill = Study)) +
  geom_hline(yintercept = 0.1, color = "grey", size = 1, linetype = "solid", alpha = 0.6)+
  geom_hline(yintercept = 0.2, color = "grey", size = 1, linetype = "solid", alpha = 0.6)+
  geom_hline(yintercept = 0.3, color = "grey", size = 1, linetype = "solid", alpha = 0.6)+
  geom_hline(yintercept = 0.4, color = "grey", size = 1, linetype = "solid", alpha = 0.6)+
  geom_col() +
  scale_fill_met_d("Redon")+
 scale_y_continuous(
    limits = c(-0.2, 0.45),  
    breaks = c(0.1, 0.2, 0.3, 0.4),
    expand = c(0,0)
  ) +
  # Annotate custom scale inside plot
  annotate(
    geom = "text", 
    x = 0.1, 
    y = 0.32, 
    label = "0.3", 
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", 
    x = 0.1, 
    y = 0.42, 
    label = "0.4", 
    color = "black", size = 6
  ) +
  annotate(
    geom = "text", 
    x = 0.1, 
    y = 0.22, 
    label = "0.2", 
    color = "black",  size = 6
  ) +
  annotate(
    geom = "text", 
    x = 0.1, 
    y = 0.12, 
    label = "0.1", 
    color = "black",  size = 6
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank(),
  ) +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14),    # Adjust the size of the legend text
        legend.title = element_text(size = 16),   # Adjust the size of the legend title
        legend.key.size = unit(1.5, "lines"))+ 
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))+ # Adjust the size of the legend keys
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y= C_Estimate+0.02, label= Trait, hjust=hjust), 
            color="black", alpha=0.8, fontface="bold", size=5, angle= label_data$angle, 
            inherit.aes = FALSE ) +
  annotate(geom = "text", x = 4, y = -0.08, label = "Literacy", color = "black",  size = 5, fontface="bold") +
  annotate(geom = "text", x = 10.5, y = -0.07, label = "Numeracy", color = "black",  size = 5, fontface="bold")+
  annotate(geom = "text",  x = 15.5,  y = -0.11,  label = "Science\nand\nHumanities",  # Break the text into three lines 
  color = "black",   size = 5, fontface="bold") +
  geom_segment(data=base_data, aes(x = start, y = -0.01, xend = end, yend = -0.01), colour = "black", alpha=0.8, size=0.8 , inherit.aes = FALSE )  

plt



