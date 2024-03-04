###### NSE Cog Paper, Figure 1. Showing Decline in Shared Environment within g######
setwd("C:/Users/Kaito/Desktop/NSE cog/cognitive ability (Figure 1)")

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

# Import Data
g_h2 <- read_excel("Figure 1 data.xlsx")
g_h2$Study <- as.character(g_h2$Study)

# Remove first four rows (unnecessary data)
g_h2 <- g_h2 %>% slice(-1:-4)

# Convert rows to numeric
g_h2$C <- as.numeric(g_h2$C)

# Select Polderman data
polderman <- g_h2 %>% filter(Study == "Polderman et al. (2015)")

# Reorder age factor
polderman$Age <- as.factor(polderman$Age)
polderman$Age <- factor(polderman$Age, levels = c("0-11", "12-17", "18-64", "65+"))

# Create Panel A
p1 <- ggplot(polderman, aes(x = Age, y = C, group = 1)) + 
  geom_line( color="grey", size = 3) +  
  geom_point(shape=21, color="black", fill="#69b3a2", size=8) +  
  labs(
    x = "",
    y = "Shared Environmental Component"
  ) +
  scale_y_continuous(limits = c(0, 0.35))+
  theme_ipsum_rc(base_size = 25, axis_title_size = 25, axis_title_just = "c")+   # Minimal theme
theme(axis.title.y = element_text(margin = margin(r = 20, unit = "pt")))
      
# Display Panel A
print(p1)






