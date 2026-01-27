# =============================================================================
# Script to Generate the Plots of Chapter 5 Exploring the Time Zones Structure
# =============================================================================
# This script explores the new Time Zones structure created after processing the
# two CSV files containing PEV charging profiles (PEV_L1.csv and PEV_L2.csv).
# This structure categorizes power demand into four time zones: Shoulder 1, Peak,
# Shoulder 2, and Off Peak. The resulting bar plots visualize the number of 
# charges per day and per week, grouped by time zone, to assess the uniformity of
# of charging behavior.

# Load Packages
# =============================================================================
library(ggplot2)
library(lubridate)
library(dplyr)

# Set Current Working Directory (adjust path as needed)
# =============================================================================
setwd("../data") # Path to the /data directory

# Prepare Plots
# =============================================================================
plot1 <- function(df, type) {
  # Number of charges per day with L1/L2 charging grouped by time zone 
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  df %>% filter(Charge_Type == type) %>% 
    ggplot(aes(x = Day, fill = Time_Zone)) + geom_bar() +
    labs(fill = "Time Zones:") +
    myfill + mytheme
}

plot2 <- function(df, type) {
  # Number of charges per week with L1/L2 charging grouped by time zone
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  df %>% filter(Charge_Type == type & (Month == "January" | Month == "March" | Month == "August")) %>% 
    ggplot(aes(x = Week, fill = Time_Zone)) + geom_bar() +
    facet_grid(. ~ Month) +
    labs(fill = "Time Zones:") +
    myfill + mytheme
}

plot3 <- function(df, type) {
  # Number of charges per week with L1/L2 charging grouped by time zone (proportions)
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  df %>% filter(Charge_Type == type & (Month == "January" | Month == "March" | Month == "August")) %>% 
    ggplot(aes(x = Week, fill = Time_Zone)) + geom_bar(position = "fill") + 
    ylab("proportions") +
    facet_grid(. ~ Month) +
    labs(fill = "Time Zones:") +
    myfill + mytheme
}

# Prepare the Time Zones Data Frame
# =============================================================================
TZ <- read.csv2(file.path("time_zones", "TimeZones_full.csv"), stringsAsFactors = FALSE) # Use "TimeZones_full.csv" for full data
TZ <- TZ %>%
  mutate(
    Date = dmy(Date),
    Day = factor(format(Date, "%a"), levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    Month = factor(format(Date, "%B"), levels = month.name),
    Week = paste("Week", as.integer((day(Date) - 1) / 7) + 1, sep = " ") # Group days into weeks
  )

# Display Plots
# =============================================================================
plot1(TZ, "L1") # Number of charges per day with L1 charging grouped by time zone 
plot1(TZ, "L2") # Number of charges per day with L2 charging grouped by time zone 

# == 
plot2(TZ, "L1") # Number of charges per week with L1 charging grouped by time zone
plot2(TZ, "L2") # Number of charges per week with L2 charging grouped by time zone

# == 
plot3(TZ, "L1") # Number of charges per week with L1 charging grouped by time zone (proportions)
plot3(TZ, "L2") # Number of charges per week with L2 charging grouped by time zone (proportions)
