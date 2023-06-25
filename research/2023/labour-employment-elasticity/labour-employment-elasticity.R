# Load required packages
library(ggplot2)
library(scales)


library(showtext)
font_add_google("Inter", "Inter")

theme_set(theme_classic(base_family = "Inter"))
showtext_auto()


# Labour Absorption Rate (LAR)
data <- data.frame(
  Year = c(2014, 2015, 2016, 2017, 2018, 2020),
  LAR = c(67.1, 65.6, 64.1, 64.1, 60.2, 38.1))

# Convert Year column to factor with all years included
data$Year <- factor(data$Year, levels = min(data$Year):max(data$Year))

# Plot the bar chart with value display
ggplot(data, aes(x = Year, y = LAR)) +
  geom_bar(stat = "identity", colour = "black", fill = "#EE746D", 
           linewidth = 0.3, width = 0.65) +
  geom_text(aes(label = LAR), vjust = -0.5, color = "black", size = 5, family="Inter") +
  labs(x = "Year", y = "Labour absorption rate (%)", fill = "") +
  scale_y_continuous(limits = c(0, 100), breaks=breaks_extended(6)) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.grid.major.y = element_line(color = "gray90", linetype = 1, linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "none")
ggsave(height=8, width=12, dpi=300, file="figures/labour-absorption-rate.svg")



# Bar chart with clustered bars for full time and part time
data <- data.frame(
  Year = factor(rep(c(2014, 2015, 2016, 2017, 2018, 2020), each=2)),
  Value = c(54.2, 51.9, 48.4, 45.6, 44.4, 25, 12.8, 13.7, 15.7, 15.8, 15.8, 13),
  Type = rep(c("Full time", "Part time"), times=6))

# Bar chart with clustered bars for full time and part time
ggplot(data, aes(x = Year, y = Value, fill=Type)) + 
  geom_bar(stat = "identity", position = "dodge", 
           color = "black", linewidth=0.2, show.legend = FALSE)  +
  geom_text(aes(factor(Year), label = Value), vjust = -0.5, color = "black", 
            size=5, position = position_dodge(0.95)) +
  facet_wrap(~Type) +
  scale_y_continuous(limits = c(0, 70), breaks=breaks_extended(7)) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.grid.major.y = element_line(color = "gray90", linetype = 1, linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA),
        strip.text = element_text(size = 18, hjust = 0.5),
        legend.position = "none",
        strip.background = element_blank()) +
  ylab("Labour absorption rate (%)")
ggsave(height=8, width=12, dpi=300, file="figures/lar-employment-type.svg")


# figure 3

data <- data.frame(Zone = c("South West", "North Central", "South South", "North West", "South East", "North East"),
                   LAR = c(47.7, 43.5, 38, 32.7, 32.4, 29.1))

# Reorder the levels of Zone based on LAR_Proportion values
data$Zone <- factor(data$Zone, levels = data$Zone[order(data$LAR)])

# Plot the bar chart with value display
ggplot(data, aes(x = Zone, y = LAR)) +
  geom_bar(stat = "identity", colour = "black", fill = "#EE746D", 
           linewidth = 0.3, width = 0.65) +
  geom_text(aes(label = LAR), vjust = -0.5, color = "black", size = 6, position = position_dodge(width = 0.8)) +
  labs(x = "Zone", y = "Labour absorption rate (%)") +
  scale_y_continuous(limits = c(0, 60), breaks=breaks_extended(6)) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.grid.major.y = element_line(color = "gray90", linetype = 1, linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA),
        strip.text = element_text(size = 18, hjust = 0.5),
        legend.position = "none",
        strip.background = element_blank())
ggsave(height=8, width=12, dpi=300, file="figures/lar-zone-nigeria.svg")


# figure 4
data <- data.frame(
  Year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
  Elasticity = c(0, 0.3, -0.3, 0.5, 1, 0.3, -0.6, -1.3, 0.5, 0, 0, 7.4, 0))

ggplot(data, aes(x = Year, y = Elasticity)) +
  geom_line(color = "#EE746D", size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha=0.5) +
  scale_y_continuous(limits = c(-2, 8), breaks=breaks_extended(11)) +
  scale_x_continuous(breaks = data$Year) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.grid.major.y = element_line(color = "gray90", linetype = 1, linewidth = 0.2),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "none")
ggsave(height=8, width=12, dpi=300, file="figures/employment-elasticity-time.svg")

