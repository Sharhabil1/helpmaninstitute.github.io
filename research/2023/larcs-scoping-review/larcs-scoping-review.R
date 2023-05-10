library(tidyverse)
library(ggplot2)
library(cowplot)
library(sf)
library(showtext) 
library(scales)
library(svglite)

theme_set(theme_classic(base_family = "helvetica"))
font_add("helvetica",   "resources/HelveticaNeueRg.ttf")
font_add("helveticaMd",   "resources/HelveticaNeueMd.ttf")
showtext_auto()

ngaShape <- st_read("data/NGA_population_v2_0_admin/NGA_population_v2_0_admin_level1_boundaries.shp")
larcsState <- read_csv("data/larcs_state.csv")

ngaShape <- ngaShape %>% 
  mutate(perc_larcs = larcsState$percent_total)

ggplot(ngaShape) + 
  geom_sf(mapping = aes(fill = perc_larcs), linewidth = 0.2, color = "black") +
  scale_fill_gradient2(name = "LARCs use", low="#ffffff", mid="#a8ddb5", high="#08589e", 
                       midpoint = mean(ngaShape$perc_larcs), labels = percent, 
                       limits = c(-0.01, 0.21), breaks=seq(0, 0.20, 0.05)) +
  panel_border(color = "#444444", size = 0.5, linetype = 1) +
  theme(axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        axis.line         = element_blank(),
        panel.grid.major  = element_blank(),
        legend.position = c(0.9, 0.15),
        legend.background = element_rect(fill="transparent"),
        legend.text = element_text(size=16, family = "helvetica"),   # change legend text font size
        legend.title = element_text(size=16, family = "helveticaMd"),        # change legend title font size
        legend.key.size = unit(0.75, 'cm'),      # change legend key size
        legend.key.height = unit(1.00, 'cm'),
        legend.spacing.y = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, "cm"))
ggsave(height=10, width=12, dpi=300, file="figures/larcs-use-map.svg")
ggsave(height=10, width=12, dpi=300, file="figures/larcs-use-map.pdf")

