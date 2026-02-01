# =============================================================================
# Script to Generate the Plots of Chapter 4 for Exploratory Data Analysis
# =============================================================================
# This script further explores the structure of the household and PEV data to
# uncover patterns, relationships, and anomalies in power demand.
# The following visualizations were created to facilitate this exploration:
#  - A bar chart displaying the monthly sum of power measurements (relative).
#  - Two line and ribbon plots displaying the average power demand on weekdays and 
#    weekends.
#  - Two bar charts displaying the average power demand on weekdays and weekends.
#  - Two box plots displaying the distribution of average power demand on weekdays 
#    and weekends.

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
# 'Time'    : Timestamp (in 10-minute intervals throughout 2010).
# 'Day_Time': Combination of day number (1-7), time (HH:MM:SS), and abbreviated day
#             for correct grouping and calculations (e.g., "1 12:30:00 Mon").
# 'Demand'  : Total power demand for each timestamp (W).
tot.demand <- function(df, file) {
  pev <- demand(file) # Calculate PEV demand
  df %>%
    mutate(
      Demand_PEV = pev$Demand,
      Demand = rowSums(cbind(Demand, Demand_PEV))
    ) %>%
    select(-Demand_PEV)
}

# Sums the power measurements of all households or households + PEVs per month.
#
# Returns a data frame with the following columns:
# 'Month'  : Month of the year (abbreviated). 
# 'Demand' : Sum of power measurements per month (W, relative).
# 'Type'   : Type of data ("Household", "L1 Charging", "L2 Charging").
sum.month.demand <- function(df, type) {
  df %>%
    mutate(Month = format(Time, "%b")) %>%
    group_by(Month) %>%
    summarize(Demand = sum(Demand), .groups = "drop") %>% 
    mutate(Type = type)
}

# Calculates the average demand of all households or PEVs for each timestamp,
# grouped by 10-minute intervals for each day of the week. For example, the
# first entry, "2010-01-04 00:00:00", represents all Mondays at midnight, and
# the last entry, "2010-01-10 23:50", represents all Sundays 10 minutes before
# Monday midnight.
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals for each day of the week).
# 'Demand' : Average power demand for each timestamp (W).
# 'Type'   : Type of data ("Household", "L1 Charging", "L2 Charging").
avg.demand <- function(file, type) {
  df <- demand(file) # Calculate household or PEV demand
  df %>%
    group_by(Day_Time) %>%
    summarize(Demand = mean(Demand), .groups = "drop") %>%
    mutate(
      Day_Time = seq(from = ymd_hm("2010-01-04 00:00"), by = "10 mins", length.out = n()),
      Type = type
    ) %>%
    rename(Time = Day_Time)
}

# Filters data based on the specified type of day ("Weekday" or "Weekend").
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals for each day of the week).
# 'Demand' : Power demand for each timestamp (W).
# 'Type'   : Type of data ("Household", "L1 Charging", "L2 Charging").
# 'Day'    : Day of the week (abbreviated).
filter.by.daytype <- function(df, type) {
  df %>%
    mutate(
      Day = format(Time, "%a"),
      Day_Type = case_when(
        Day %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ "Weekday",
        Day %in% c("Sat", "Sun") ~ "Weekend"
      )
    ) %>%
    filter(Day_Type == type) %>%
    select(-Day_Type)
}

# Prepare Plots
# =============================================================================
plot1 <- function(df) {
  # Monthly sum of power measurements (relative)
  dark2_colors <- brewer.pal(3, "Dark2")
  
  myfill <- scale_fill_manual(values = c(dark2_colors[2], dark2_colors[3], dark2_colors[1]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = Month, y = Demand, fill = Type)) + 
    geom_bar(stat = "identity", position = "dodge") +
    labs(y = "Power Sum (W)") +
    myfill + mytheme
}

plot2 <- function(df) {
  # Average yearly power demand on weekdays/weekends
  dark2_colors <- brewer.pal(3, "Dark2")
  
  myfill <- scale_fill_manual(values = c(dark2_colors[2], dark2_colors[3], dark2_colors[1]))
  mycolor <- scale_color_manual(values = c("black", "black", "black"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Time, y = Demand)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = Demand, fill = Type), alpha = .6) +
    facet_grid(. ~ Type) +
    scale_x_datetime(date_labels = "%a") +
    labs(y = "Avg. Power Demand (W)") +
    myfill + mycolor + mytheme
}

plot3 <- function(df) {
  # Average yearly power demand on weekdays/weekends (bar chart)
  dark2_colors <- brewer.pal(7, "Dark2")
  
  myfill <- scale_fill_manual(values = c(dark2_colors[3], dark2_colors[2], dark2_colors[1], dark2_colors[7], dark2_colors[6]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Day, y = Demand, fill = Day)) + 
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(. ~ Type) +
    labs(y = "Avg. Power Demand (W)") +
    myfill + mytheme
}

plot4 <- function(df) {
  # Average yearly power demand on weekdays/weekends (box plot)
  dark2_colors <- brewer.pal(7, "Dark2")
  
  myfill <- scale_fill_manual(values = c(dark2_colors[3], dark2_colors[2], dark2_colors[1], dark2_colors[7], dark2_colors[6]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Day, y = Demand, fill = Day)) + 
    geom_boxplot() +
    facet_grid(. ~ Type) + 
    labs(y = "Avg. Power Demand (W)") +
    myfill + mytheme
}

# Calculate monthly power sum for household and PEV scenarios
# =============================================================================
Household <- demand(file.path("processed", "Household.csv")) # Sum household demand
Total_L1 <- tot.demand(Household, file.path("processed", "PEV_L1.csv")) # Sum household + PEV demand (L1)
Total_L2 <- tot.demand(Household, file.path("processed", "PEV_L2.csv")) # Sum household + PEV demand (L2)

Household_Month_SUM <- sum.month.demand(Household, "Household") # Monthly cumulative household load
Total_L1_Month_SUM <- sum.month.demand(Total_L1, "Household + L1") # Monthly cumulative household + PEV load (L1)
Total_L2_Month_SUM <- sum.month.demand(Total_L2, "Household + L2") # Monthly cumulative household + PEV load (L2)

# Combine the data for all scenarios into a single data frame
Month_SUM <- rbind(Total_L2_Month_SUM, Total_L1_Month_SUM, Household_Month_SUM)
Month_SUM$Type <- factor(Month_SUM$Type, levels = c("Household + L2", "Household + L1", "Household"))
Month_SUM$Month <- factor(Month_SUM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Calculate average power demand on weekdays/weekends for household and PEV scenarios
# =============================================================================
Household <- avg.demand(file.path("processed", "Household.csv"), "Household") # Avg household demand
PEV_L1 <- avg.demand(file.path("processed", "PEV_L1.csv"), "L1 Charging") # Avg PEV demand (L1)
PEV_L2 <- avg.demand(file.path("processed", "PEV_L2.csv"), "L2 Charging") # Avg PEV demand (L2)

# Combine the data for all scenarios into a single data frame
Year_AVG <- rbind(Household, PEV_L1, PEV_L2)

# Filter the data by weekdays
Weekdays <- filter.by.daytype(Year_AVG, "Weekday")
Weekdays$Type <- factor(Weekdays$Type, levels = c("L2 Charging", "L1 Charging", "Household"))
Weekdays$Day <- factor(Weekdays$Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Filter the data by weekends
Weekends <- filter.by.daytype(Year_AVG, "Weekend")
Weekends$Type <- factor(Weekends$Type, levels = c("L2 Charging", "L1 Charging", "Household"))
Weekends$Day <- factor(Weekends$Day, levels = c("Sat", "Sun"))

# Display Plots
# =============================================================================
plot1(Month_SUM) # Monthly sum of power measurements (relative)

# ==
plot2(Weekdays) # Average yearly power demand on weekdays
plot3(Weekdays) # Average yearly power demand on weekdays (bar chart)
plot4(Weekdays) # Average yearly power demand on weekdays (box plot)

# ==
plot2(Weekends) # Average yearly power demand on weekends
plot3(Weekends) # Average yearly power demand on weekends (bar chart)
plot4(Weekends) # Average yearly power demand on weekends (box plot)
