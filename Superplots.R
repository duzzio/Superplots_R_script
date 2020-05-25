library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(dplyr)
library(readxl)
windowsFonts(Arial = windowsFont("Arial"))

# Defines a colorblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Imports dataset called "combined" that has the columns "replicate," "variable," and "value"
combined <- read_excel("C:/Add/file/path/here/data.xlsx", sheet = 1)

# Orders the variables on x-axis
combined$variable <- factor(combined$variable, levels = c("Before", "After"))

# Calculates averages of each replicate
ReplicateAverages <- combined %>% group_by(variable, replicate) %>%
  summarise_each(list(mean))
ReplicateAverages

# Gives the p-value for the t-Test of variable 1 and 2
ttest1 <- t.test(x=ReplicateAverages$value[1:3], y=ReplicateAverages$value[4:6], alternative="two.sided",var.equal = TRUE)
ttest1p <- ttest1[["p.value"]]
ttest1p

# Calculates total averages
TotalAverages <- ReplicateAverages %>% summarise_each(list(mean))
TotalAverages

# Plots Superplot based on biological replicate averages
ggplot(combined, aes(x=variable,y=value,color=factor(replicate))) +

  # Adds individual data points
  geom_beeswarm(cex=3) +
  
  # Adds mean values as bars
  stat_summary(data = TotalAverages, fun.y = mean, fun.ymin = mean, fun.ymax = mean,
               geom = "crossbar", width = 0.25, color = "black") +
               
  # Adds error bars
  stat_summary(data = ReplicateAverages, fun.data = mean_se,
               geom = "errorbar", width = 0.1, color = "black", size= 1) +
               
  # Adds color palette
  scale_colour_manual(values=cbPalette) +
  
  # Adds Replicative averages as points (argument "cex" can be used to spread the data points if the averages are close together)
  geom_beeswarm(data=ReplicateAverages, size=5) +
  
  #Cosmetics and labeling
  theme_bw() + theme(axis.line = element_line(size = 1, colour = "black"),
                     legend.position = "none",
                     axis.title.y = element_text(family="Arial", size=28, color = "black", vjust = 2),
                     axis.text = element_text(family="Arial", size = 28, color = "black"),
                     axis.ticks = element_line(size = 1, color = "black"), 
                     axis.ticks.length = unit(2, "mm"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     panel.border = element_blank()) +
  xlab("") + ylab("Total counts")
