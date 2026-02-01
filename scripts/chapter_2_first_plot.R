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
library(RColorBrewer)
library(lubridate)
library(dplyr)

# Set Current Working Directory (adjust path as needed)
# =============================================================================
setwd("../data") # Path to the /data directory

# Define Functions
# =============================================================================
# Sums the power demand of all households or PEVs for each timestamp.
#
# Returns a data frame with the following columns:
# 'Time'     : Timestamp (in 10-minute intervals throughout 2010).
# 'Day_Time' : Combination of day number (1-7), time (HH:MM:SS), and abbreviated day
#              for correct grouping and calculations (e.g., "1 12:30:00 Mon").
# 'Demand'   : Total power demand for each timestamp (W).
demand <- function(file) {
  df <- read.csv2(file)
  df %>%
    mutate(
      Demand = rowSums(select(., -Time)),
      Time = dmy_hm(Time),
      Day_Name = format(Time, "%a"),
      Day_Num = wday(Time, week_start = 1),
      Hour = format(Time, "%H:%M:%S"),
      Day_Time = paste(Day_Num, Hour, Day_Name, sep = " ")
    ) %>%
    select(Time, Day_Time, Demand)
}

# Sums the power demand of all households (df) + PEVs (file) for each timestamp.
#
# Returns a data frame with the following columns:
# 'Time'     : Timestamp (in 10-minute intervals throughout 2010).
# 'Day_Time' : Combination of day number (1-7), time (HH:MM:SS), and abbreviated day
#              for correct grouping and calculations (e.g., "1 12:30:00 Mon").
# 'Demand'   : Total power demand for each timestamp (W).
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
# 'Demand' : Average power demand for each timestamp, grouped by day (W).
# 'Type'   : Type of data ("Household", "Household + L1", "Household + L2").
avg.demand <- function(df, type) {
  df %>%
    group_by(Day_Time) %>%
    summarize(Demand = mean(Demand), .groups = "drop") %>%
    mutate(
      Day_Time = seq(ymd_hm("2010-04-01 00:00"), by = "10 mins", length.out = n()),
      Type = type
    ) %>%
    rename(Time = Day_Time)
}

# Prepare Plots
# =============================================================================
plot1 <- function(df) {
  # Average yearly power demand by day
  set1_colors <- brewer.pal(3, "Dark2")
  
  myfill <- scale_fill_manual(values = c(set1_colors[2], set1_colors[3], set1_colors[1]))
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
    labs(y = "Avg. Power Demand (W)") +
    myfill + mycolor + mytheme
}

# Calculate average yearly power demand by day for household and PEV scenarios
# =============================================================================
Household <- demand(file.path("processed", "Household.csv")) # Sum household demand
Total_L1 <- tot.demand(Household, file.path("processed", "PEV_L1.csv")) # Sum household + PEV demand (L1)
Total_L2 <- tot.demand(Household, file.path("processed", "PEV_L2.csv")) # Sum household + PEV demand (L2)

Household_Year_AVG <- avg.demand(Household, "Household") # Avg household demand
Total_L1_Year_AVG <- avg.demand(Total_L1, "Household + L1") # Avg household + PEV demand (L1)
Total_L2_Year_AVG <- avg.demand(Total_L2, "Household + L2") # Avg household + PEV demand (L2)

# Combine the data for all scenarios into a single data frame
Year_AVG <- rbind(Total_L2_Year_AVG, Total_L1_Year_AVG, Household_Year_AVG)
Year_AVG$Type <- factor(Year_AVG$Type, levels = c("Household + L2", "Household + L1", "Household"))

# Display Plots
# =============================================================================
plot1(Year_AVG) # Average yearly power demand by day
