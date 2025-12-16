# =============================================================================
# Script to Generate the Introductory Plot of Chapter 2
# =============================================================================
# This script visualizes the average yearly power demand by day for households 
# and plug-in electric vehicle (PEV) charging with two charging types:
#  - Level 1 (L1): low power demand, slow charging.
#  - Level 2 (L2): high power demand, fast charging. 

# Load Packages
# =============================================================================
library(ggplot2)
library(lubridate)
library(dplyr)

# Set Current Working Directory (adjust path as needed)
# =============================================================================
setwd("D:/Informatics/BigData") # Path to data files (CSV)

# Define Functions
# =============================================================================
# Sums the power demand of all households or PEVs for each timestamp.
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals for the year of 2010).
# 'DayTime': Combination of day number (1-7), time (HH:MM:SS), and abbreviated day
#            for correct grouping and calculations (e.g., "1 12:30:00 Mon").
# 'Demand' : Total power demand for each timestamp.
demand <- function(file) {
  df <- read.csv2(file)
  df %>%
    mutate(
      Demand = rowSums(select(., -Time)),
      Time = dmy_hm(Time),
      DayName = format(Time, "%a"),
      DayNum = recode(DayName, "Mon" = "1", "Tue" = "2", "Wed" = "3", "Thu" = "4", "Fri" = "5", "Sat" = "6", "Sun" = "7"),
      Hour = format(Time, "%H:%M:%S"),
      DayTime = paste(DayNum, Hour, DayName, sep = " ")
    ) %>%
    select(Time, DayTime, Demand)
}

# Sums the power demand of all households (df) + PEVs (file) for each timestamp.
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals for the year of 2010).
# 'DayTime': Combination of day number (1-7), time (HH:MM:SS), and abbreviated day
#            for correct grouping and calculations (e.g., "1 12:30:00 Mon").
# 'Demand' : Total power demand for each timestamp.
tot.demand <- function(df, file) {
  pev <- demand(file) # Calculate PEV demand
  df %>%
    mutate(
      Demand_PEV = pev$Demand,
      Demand = rowSums(cbind(Demand, Demand_PEV))
    ) %>%
    select(-Demand_PEV)
}

# Calculates the average demand of all households or households + PEVs for each 
# timestamp, grouped by 10-minute intervals for each day of the week. For 
# example, the first entry, "2010-01-04 00:00:00", represents all Mondays at
# midnight, and the last entry, "2010-01-10 23:50", represents all Sundays 10
# minutes before Monday midnight.
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals for each day of the week).
# 'Demand' : Average power demand for each timestamp, grouped by day.
# 'Type'   : Type of data ("House", "House + L1", "House + L2").
avg.demand <- function(df, type) {
  df %>%
    group_by(DayTime) %>%
    summarize(Demand = mean(Demand), .groups = "drop") %>%
    mutate(
      DayTime = seq(ymd_hm("2010-1-4 00:00"), by = "10 mins", length.out = n()),
      Type = type
    ) %>%
    rename(Time = DayTime)
}

# Prepare Plots
# =============================================================================
plot1 <- function(df) {
  # Average yearly power demand by day
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4"))
  mycolor <- scale_color_manual(values = c("black", "black", "black"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = Time, y = Demand, color = Type)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = Demand, fill = Type), alpha = .6) +
    scale_x_datetime(date_labels = "%a", breaks = "1 day") +
    labs(y = "Average demand (W)") +
    myfill + mycolor + mytheme
}

# Calculate average yearly power demand by day for household and PEV scenarios
# =============================================================================
House <- demand("House.csv") # Sum household demand
Total_L1 <- tot.demand(House, "PEV_L1.csv") # Sum household + PEV demand (L1)
Total_L2 <- tot.demand(House, "PEV_L2.csv") # Sum household + PEV demand (L2)

House_Year_AVG <- avg.demand(House, "House") # Avg household demand
Total_L1_Year_AVG <- avg.demand(Total_L1, "House + L1") # Avg household + PEV demand (L1)
Total_L2_Year_AVG <- avg.demand(Total_L2, "House + L2") # Avg household + PEV demand (L2)

# Combine the data for all scenarios into a single data frame
Year_AVG <- rbind(Total_L2_Year_AVG, Total_L1_Year_AVG, House_Year_AVG)
Year_AVG$Type <- factor(Year_AVG$Type, levels = c("House + L2", "House + L1", "House"))

# Display Plots
# =============================================================================
plot1(Year_AVG) # Average yearly power demand by day
