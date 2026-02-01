# =============================================================================
# Script to Generate the Plots of Chapter 5 Exploring the Time Zones Structure
# =============================================================================
# This script explores the Time Zones structure created after processing the PEV 
# charging profiles. It categorizes each weekday charge into one of four time 
# zones: Shoulder 1 (7:00-13:50), Peak (14:00-19:50), Shoulder 2 (20:00-21:50), 
# and Off-Peak (22:00-6:50). Weekend charges are categorized into two zones:
# Shoulder (7:00-21:50) and Off-Peak (22:00-6:50). 
# The resulting bar plots show the number of charges per day and per week, 
# grouped by time zone, to assess the uniformity of charging behavior.

# Load Packages
# =============================================================================
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(dplyr)

# Set Current Working Directory (adjust path as needed)
# =============================================================================
setwd("../data") # Path to the /data directory

# Prepare Plots
# =============================================================================
plot1 <- function(df, type) {
  # Number of L1/L2 charges per day by time zone
  dark2_colors <- brewer.pal(7, "Dark2")
  
  myfill <- scale_fill_manual(values = c(dark2_colors[3], dark2_colors[2], dark2_colors[1], dark2_colors[7], dark2_colors[6]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  df %>% filter(Charge_Type == type) %>% 
    ggplot(aes(x = Day, fill = Time_Zone)) + geom_bar() +
    ylab("Charge Count") +
    labs(fill = "Time Zone") +
    myfill + mytheme
}

plot2 <- function(df, type) {
  # Number of L1/L2 charges per week by time zone
  dark2_colors <- brewer.pal(7, "Dark2")
  
  myfill <- scale_fill_manual(values = c(dark2_colors[3], dark2_colors[2], dark2_colors[1], dark2_colors[7], dark2_colors[6]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  df %>% filter(Charge_Type == type & (Month == "January" | Month == "March" | Month == "August")) %>% 
    ggplot(aes(x = Week, fill = Time_Zone)) + geom_bar() +
    ylab("Charge Count") +
    facet_wrap(~ Month, scales = "free_x") +
    labs(fill = "Time Zone") +
    myfill + mytheme
}

plot3 <- function(df, type) {
  # Number of L1/L2 charges per week by time zone (proportions)
  dark2_colors <- brewer.pal(7, "Dark2")
  
  myfill <- scale_fill_manual(values = c(dark2_colors[3], dark2_colors[2], dark2_colors[1], dark2_colors[7], dark2_colors[6]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  df %>% filter(Charge_Type == type & (Month == "January" | Month == "March" | Month == "August")) %>% 
    ggplot(aes(x = Week, fill = Time_Zone)) + geom_bar(position = "fill") + 
    ylab("Proportions") +
    facet_wrap(~ Month, scales = "free_x") +
    labs(fill = "Time Zone") +
    myfill + mytheme
}

# Prepare the Time Zones Data Frame
# =============================================================================
TZ <- read.csv2(file.path("time_zones", "TimeZones_full.csv"), stringsAsFactors = FALSE) # Use "TimeZones_full.csv" for full data
TZ <- TZ %>%
  mutate(
    Charge_Date = dmy(Charge_Date),
    Day = factor(format(Charge_Date, "%a"), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    Month = factor(format(Charge_Date, "%B"), levels = month.name),
    Week = paste("Week", ceiling(day(Charge_Date) / 7)) # Group days into weeks
  )

# Display Plots
# =============================================================================
plot1(TZ, "L1") # Number of L1 charges per day by time zone
plot1(TZ, "L2") # Number of L2 charges per day by time zone

# == 
plot2(TZ, "L1") # Number of L1 charges per week by time zone
plot2(TZ, "L2") # Number of L2 charges per week by time zone

# == 
plot3(TZ, "L1") # Number of L1 charges per week by time zone (proportions)
plot3(TZ, "L2") # Number of L2 charges per week by time zone (proportions)
