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




##### knowledge-of-larcs #####

data <- data.frame(
  Method = c("Implants", "Injectables", "Intrauterine devices"),
  Year = factor(rep(c(2003, 2008, 2013, 2018), each=3)),
  Value = c(10.4, 57.1, 27.1, 10.1, 50.9, 24.9, 24.7, 68.3, 31.8, 70.3, 82.1, 45.8)
)


ggplot(data, aes(x = factor(Method), y = Value, fill = Year)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5, 
           color = "black", linewidth=0.2, show.legend = FALSE)  +
  geom_text(aes(Method, label = Value), vjust = -0.5, color = "black", 
            size=5, position = position_dodge(0.95)) +
  geom_text(aes(label=Year, y = 0), vjust = 1.25,  
            size=6, position = position_dodge(0.95)) +
  labs(x="LARC methods", y="Percentage with knowledge of LARCs (%)") +
  ylim(0, 100) +
  scale_fill_manual(values = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linetype = 1, linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
        legend.position = c(0.1, 0.75))
ggsave(height=10, width=12, dpi=300, file="figures/knowledge-of-larcs.svg")
ggsave(height=10, width=12, dpi=300, file="figures/knowledge-of-larcs.pdf")




##### utilization-of-larcs #####

data <- data.frame(
  Method = c("Implants", "Injectables", "Intrauterine devices"),
  Year = factor(rep(c(2003, 2008, 2013, 2018), each=3)),
  Value = c(0.0, 2.0,	0.7, 0.0,	2.6, 1.0, 0.4, 3.2,	1.1, 3.4,	3.2, 0.8))

ggplot(data, aes(x = factor(Method), y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5,
           color = "black", linewidth=0.2, show.legend = FALSE)  +
  geom_text(aes(Method, label = Value), vjust = -0.5, color = "black",
            size=5, position = position_dodge(0.95)) +
  geom_text(aes(label=Year, y = 0), vjust = 1.25,
            size=6, position = position_dodge(0.95)) +
  labs(x="LARC methods", y="Percentage using LARCs (%)") +
  ylim(0, 5) +
  scale_fill_manual(values = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linetype = 1, linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
        legend.position = c(0.1, 0.75))
ggsave(height=10, width=12, dpi=300, file="figures/utilization-of-larcs.svg")
ggsave(height=10, width=12, dpi=300, file="figures/utilization-of-larcs.pdf")





##### larcs-use-map #####

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

