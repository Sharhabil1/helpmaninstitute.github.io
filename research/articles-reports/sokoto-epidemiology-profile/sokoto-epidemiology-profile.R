
library(ggplot2)
library(stringr)
library(svglite)

#Fig 2
data <- data.frame(
  Label = c("Diabetes", "Other NCDs less Cancer", "CVDs", "Others", "Communicable Diseases, Maternal, Perinatal and Neonatal"),
  Prop_Mort = c(2, 9, 10, 4, 75)
)

colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0")

# Create the pie chart
chart <- ggplot(data, aes(x = "", y = Prop_Mort, fill = Label)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Prop_Mort, "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = colors) +
  labs(x = NULL, y = NULL, fill = "Mortality Causes") +
  theme_void() +
  theme(plot.background = element_rect(color = "black", size = 1))
# Display the chart
print(chart)

ggsave("figures/proportional-mortality-sokoto.svg", height=10, width=14, dpi = 300)
ggsave("figures/proportional-mortality-sokoto.pdf", height=10, width=14, dpi = 300)


#Figure 3
# Create a data frame with the given data
data <- data.frame(
  Years = c("1992", "1994", "1999", "2001", "2003", "2005", "2008", "2010", "2018"),
  Prevalence = c(1.8, 1.6, 2.7, 2.8, 4.5, 3.2, 6, 3.3, 0.4)
)
# Create a numeric sequence for the x-axis
data$X <- seq_along(data$Years)

# Create the line chart with joined scattered points using ggplot2
ggplot(data, aes(x = X, y = Prevalence)) +
  geom_line(linewidth = 1, color = "#2c7fb8") +
  geom_point(size = 3, color = "#08306b") +
  geom_text(aes(label = paste0(Prevalence, "%")), vjust = -1, size = 4) +  # Add labels with percentages
  scale_x_continuous(
    breaks = data$X,
    labels = data$Years
  ) +
  scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0, 7), labels = function(x) sprintf("%.1f%%", x)) +
  labs(x = "Year", y = "Prevalence") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0.5, size = 14, face = "bold"), 
        axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),  # Adjust x-axis label font size
        axis.title.y = element_text(size = 14)) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.grid.major.x = element_blank(),  # Remove vertical gridlines
        panel.grid.minor = element_blank()) 

ggsave("figures/hiv-prevalence-sokoto.svg", height=10, width=14, dpi = 300)
ggsave("figures/hiv-prevalence-sokoto.pdf", height=10, width=14, dpi = 300)




##### Figure 4 ##### 
data <- data.frame(
  Label = c("Mycobacterium tuberculosis", "Mycobacterium bovis", "Atypical mycobacterium"),
  Prop_Mort = c(69, 21, 10))

data$Label <- factor(data$Label, levels = data$Label[order(-data$Prop_Mort)])

my_colors <- c("#f0f9e8", "#dfc27d", "#7bccc4")

ggplot(data, aes(x = Label, y = Prop_Mort, fill = Label)) +
  geom_bar(stat = "identity", linewidth = 0.01, width = 0.6, fill = my_colors, color = "black") +
  geom_text(aes(label = paste0(Prop_Mort, "%")), vjust = -0.5, color = "black", size = 5) +  # Add percentages at the top of the bars
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(x = "Mycobacterium species", y = "Distribution") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) 
ggsave("figures/mycobacterium-specie-distribution.svg", height=10, width=12, dpi = 300)
ggsave("figures/mycobacterium-specie-distribution.pdf", height=10, width=12, dpi = 300)




##### Figure 5 ##### 
data <- data.frame(
  Year <- c(2006, 2008, 2009, 2010, 2011, 2012, 2013),
  Cases <- c(1383, 1912, 2212, 1905, 2372, 2689, 3329))

my_colors <- c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe")

ggplot(data, aes(x = factor(Year), y = Cases)) +
  geom_bar(stat = "identity", fill = my_colors, color = "black", width = 0.8) +
  xlab("Year") +
  ylab("Number of tuberculosis cases") +
  scale_y_continuous(breaks = seq(0, 3500, 500), limits = c(0, 3500), labels = scales::comma) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("figures/tuberculosis-cases-sokoto.svg", height=10, width=12, dpi = 300)
ggsave("figures/tuberculosis-cases-sokoto.pdf", height=10, width=12, dpi = 300)




##### Figure 6 ##### 
data <- data.frame(
  Year <- c(2009, 2010, 2011, 2012, 2013),
  Cases <- c(225, 193, 132, 242, 86))

my_colors <- c("#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3")

ggplot(data, aes(x = Year, y = Cases)) +
  geom_bar(stat = "identity", fill = my_colors, color = "black", width = 0.8) +
  labs(x = "Year", y = "Number of leprosy cases") +
  geom_text(aes(label = Cases, vjust = -1, size = 4)) +
  scale_y_continuous(breaks = seq(0, 300, 50), limits = c(0, 300)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("figures/leprosy-cases-sokoto.svg", height=10, width=12, dpi = 300)
ggsave("figures/leprosy-cases-sokoto.pdf", height=10, width=12, dpi = 300)




##### Figure 7 #####
data <- data.frame(
  Year <- c(2004, 2005, 2006, 2007, 2008),
  Cases <- c(77439, 107281, 73308, 72410, 46714))

ggplot(data, aes(x = Year, y = Cases/1000)) +
  geom_bar(stat = "identity", fill = my_colors, color = "black", width = 0.7) +
  labs(x = "Year", y = "Number of malaria cases (thousands)") +
  geom_text(aes(label = round(Cases/1000,0), vjust = -1, size = 4)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())  +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, by = 20))

ggsave("figures/malaria-cases-sokoto.svg", height=10, width=12, dpi = 300)
ggsave("figures/malaria-cases-sokoto.pdf", height=10, width=12, dpi = 300)




##### Figure 8 #####
data <- data.frame(Year = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016),
                   Deaths = c(35894, 31277, 24790, 19301, 16126, 13331, 11735, 12400, 12988, 14129, 15454))

# Create the line graph with adjusted scale, x-axis labels, and border line
ggplot(data, aes(x = Year, y = Deaths/1000)) +
  geom_line(size = 1.2, color = "#2c7fb8") +  # Change the size and color of the line
  geom_point(color = "#08306b", size = 3.5) +  # Change the color and size of the points
  labs(x = "Year",  y = "Number of malaria-related deaths (thousands)") +
  scale_y_continuous(breaks = seq(0, 40, by = 5),
                     limits = c(0, 40)) +
  scale_x_continuous(breaks = data$Year, labels = data$Year) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/malaria-deaths-sokoto.svg", height=10, width=12, dpi = 300)
ggsave("figures/malaria-deaths-sokoto.pdf", height=10, width=12, dpi = 300)




##### Figure 9 #####
data <- data.frame(
  Year = c(2003, 2008, 2013, 2018),
  Prevalence = c(18.8, 14, 5, 12.8))

ggplot(data, aes(x = as.factor(Year), y = Prevalence)) +
  geom_bar(stat = "identity", fill = "#7bccc4", color = "#08306b", width = 0.6) +  # Adjust the width as desired
  geom_text(aes(label = paste0(round(Prevalence,0), "%")), vjust = -1, size = 4) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  labs(x = "Year", y = "Prevalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/diarrhea-prevalence-children.svg", height=10, width=12, dpi = 300)
ggsave("figures/diarrhea-prevalence-children.pdf", height=10, width=12, dpi = 300)




##### Figure 10 #####
data <- data.frame(
  Schistosomiasis = c("Urinary Schistosomiasis", "Other categories", "Intestinal Schistosomiasis"),
  Prevalence = c(60.8, 36.27, 2.93))

data$Schistosomiasis <- factor(data$Schistosomiasis, levels = data$Schistosomiasis[order(-data$Prevalence)])

my_colors <- c("#ccebc5", "#a8ddb5", "#7bccc4")

ggplot(data, aes(x = Schistosomiasis, y = Prevalence)) +
  geom_bar(stat = "identity", width = 0.5, fill = my_colors, color = "black") +
  labs(x = "", y = "Prevalence") +
  geom_text(aes(label = paste0(round(Prevalence,0), "%")), vjust = -1, size = 4) +
  scale_y_continuous(breaks = seq(0, 70, 10), limits = c(0, 70), labels = function(x) sprintf("%d%%", x)) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/schistosomiasis-prevalence-children.svg", height=10, width=12, dpi = 300)
ggsave("figures/schistosomiasis-prevalence-children.pdf", height=10, width=12, dpi = 300)




##### Figure 11 #####
data <- data.frame(
  Label = c("Indirect causes", "Hemorrhage", "Hypertensive disorders", "Sepsis", "Abortion", "Other direct causes", "Embolism"),
  Prop_Mort = c(26, 25, 17, 11, 11, 8, 2))

data$Label <- factor(data$Label, levels = data$Label[order(-data$Prop_Mort)])

my_colors <- c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594")

# Preprocess the labels and wrap them using str_wrap
data$Label <- str_wrap(data$Label, width = 10)

ggplot(data, aes(x = reorder(Label, -Prop_Mort), y = Prop_Mort, fill = Label)) +
  geom_bar(stat = "identity", linewidth = 0.5, fill = my_colors, color = "black", width = 0.8) +
  geom_text(aes(label = paste0(Prop_Mort, "%")), vjust = -0.5, color = "black", size = 3.5) +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 31), labels = function(x) paste0(x, "%")) +
  labs(x = "Causes",
       y = "Proportional Mortality") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/maternal-mortality-causes.svg", height=10, width=12, dpi = 300)
ggsave("figures/maternal-mortality-causes.pdf", height=10, width=12, dpi = 300)




##### Figure 12 #####
data <- data.frame(
  Years = c("2008", "2009", "2010", "2011", "2012", "2013"),
  Prevalence = c(4099, 4216, 5114, 5011, 5385, 5632))

data$X <- seq_along(data$Years)

ggplot(data, aes(x = X, y = Prevalence)) +
  geom_line(size = 1.2, color = "#2c7fb8") +  # Change the size and color of the line
  geom_point(color = "#08306b", size = 3.5) +  # Change the color and size of the points
  scale_x_continuous(breaks = data$X, labels = data$Years) +
  scale_y_continuous(breaks = seq(0, 6000, 1000), limits = c(0, 6200), labels = scales::comma) +
  labs(x = "Year", y = "Prevalence") +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/substance-abuse-sokoto.svg", height=10, width=12, dpi = 300)
ggsave("figures/substance-abuse-sokoto.pdf", height=10, width=12, dpi = 300)




##### Figure 13 #####
data <- data.frame(
  State = c("Lagos", "Oyo", "Gombe", "Imo", "Yobe", "Delta", "Adamawa", "Ogun", "Ondo", "Enugu", "Kano", "Bauchi", "Edo", "Rivers", "Bayelsa", "Osun", "Taraba", "Zamfara", "Kwara", "Ebonyi", "Kebbi", "Akwa Ibom", "Borno", "Katsina", "Ekiti",  "Nasarrawa", "Niger", "Abia", "Anambra", "Cross River", "Plateau", "FCT Abuja", "Kaduna", "Kogi", "Sokoto", "Benue", "Jigawa"),
  Prevalence = c(33, 23, 21.2, 18.1, 18, 18, 17, 17, 17, 16.3, 16, 16, 15, 15, 14, 14, 14, 13.5, 13, 12.8, 12.6, 12.5, 12, 12, 11.9, 11.8, 11.6, 11.3, 11.2, 11.2, 11, 10, 10, 9.2, 9, 8, 7))

# Reorder the levels of the "State" variable based on the "Prevalence" variable
data$State <- factor(data$State, levels = data$State[order(data$Prevalence)])

# Plotting
ggplot(data, aes(y = State, x = Prevalence)) +
  geom_bar(stat = "identity", color = "transparent", linewidth = 0.1, fill = ifelse(data$State == "Sokoto", "#dd3497", "#0868ac"), width = 0.6) +
  geom_text(aes(label = paste0(Prevalence, "%")), hjust = -0.2, color = "black", size = 10) +
  scale_x_continuous(limits = c(0, 37), expand = c(0, 0)) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.ticks.y = element_line(linewidth=0.5),
        axis.ticks.length=unit(0.1, "inch"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 24),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_rect(aes(xmin = 0, xmax = 0.01, ymin = -Inf, ymax = Inf), fill = NA, color = "black")

ggsave("figures/substance-abuse-nigeria.svg", height=26, width=22, dpi = 300)
ggsave("figures/substance-abuse-nigeria.pdf", height=26, width=22, dpi = 300)




##### Figure 14 #####
# Create a data frame with the provided information
data <- data.frame(
  Year = c("2010", "2011", "2012", "2013"),
  Killed = c(81, 55, 235, 580),
  Injured = c(68, 33, 166, 200))

# Reshape the data to long format
data_long <- tidyr::pivot_longer(data, cols = c(Killed, Injured), names_to = "Category", values_to = "Count")

# Create the stacked bar chart
ggplot(data_long, aes(x = Year, y = Count, fill = Category)) +
  geom_bar(stat = "identity", linewidth = 0.1, color = "black", width = 0.7) +
  labs(x = "Year", y = "Count", fill = "") +
  scale_fill_manual(values = c("#2c7fb8", "#de2d26")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

ggsave("figures/road-traffic-accidents.svg", height=10, width=12, dpi = 300)
ggsave("figures/road-traffic-accidents.pdf", height=10, width=12, dpi = 300)






