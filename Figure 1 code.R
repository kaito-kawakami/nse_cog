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


################################################################################
# Panel B
################################################################################
panel_b <- g_h2 %>% filter(Study != "Polderman et al. (2015)")

# Reorder age factor
panel_b$Age <- as.factor(panel_b$Age)
panel_b$Age <- factor(panel_b$Age, levels = c("4-10", "11-13", "14-34", "25"))

panel_b$Age <- recode(panel_b$Age, 
                        '4-10' = '9', # recode ages using means from Haworth
                      '11-13' = '12',
                      '14-34' = '17')


# Create ggplot for Haworth et al. with Malachini et al. extension
p2 <- ggplot(panel_b, aes(x = Age, y = C, group = 1)) +  
  geom_line( color="grey", size = 3) +  
  geom_point(shape=21, color="black", fill="#69b3a2", size=8) +  
  labs(
    x = "",
    y = "Shared Environmental Component"
  ) +
  scale_y_continuous(limits = c(0, 0.35))+
  theme_ipsum_rc(base_size = 25, axis_title_size = 25, axis_title_just = "c")+   # Minimal theme
  theme(axis.title.y = element_text(margin = margin(r = 20, unit = "pt")))
# Display the plot
print(p2)

### Combine ggplots 
library(ggpubr)

figure <- ggarrange(p1, p2,
                    labels = c("Panel A", "Panel B"),
                    font.label = list(size = 25, color = "black", face = "bold", family = NULL),
                    ncol = 2, nrow = 1)

print(figure)





