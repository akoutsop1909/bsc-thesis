# =============================================================================
# Script to Generate the Plots of Chapter 4 for Exploratory Data Analysis
# =============================================================================
# This script further explores the structure of the household and PEV data to
# uncover patterns, relationships, and anomalies in power demand consumption.
# The following visualizations were created to facilitate this exploration:
#  - A bar chart displaying the total power demand per month.
#  - Two line and ribbon plots displaying the average power demand on working and 
#    weekend days.
#  - Two bar charts displaying the average power demand on working and weekend days.
#  - Two box plots displaying the distribution of average power demand on working 
#    and weekend days.

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

# Sums the power demand of all households or households + PEVs per month.
#
# Returns a data frame with the following columns:
# 'Month'  : Month of the year (abbreviated). 
# 'Demand' : Total power demand per month.
# 'Type'   : Type of data ("Household only", "L1 charging only", "L2 charging only").
sum.month.demand <- function(df, type) {
  df %>%
    mutate(Month = format(Time, "%b")) %>%
    group_by(Month) %>%
    summarize(Demand = sum(Demand), .groups = "drop") %>% 
    mutate(Type = type)
}

# Calculates the average demand of all households or households + PEVs for each
# grouped by 10-minute intervals for each day of the week. For example, the
# first entry, "2010-01-04 00:00:00", represents all Mondays at midnight, and
# the last entry, "2010-01-10 23:50", represents all Sundays 10 minutes before
# Monday midnight.
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals for each day of the week).
# 'Demand' : Average power demand for each timestamp.
# 'Type'   : Type of data ("Household", "L1 charging only", "L2 charging only").
avg.demand <- function(file, type) {
  df <- demand(file) # Calculate household or PEV demand
  df %>%
    group_by(DayTime) %>%
    summarize(Demand = mean(Demand), .groups = "drop") %>%
    mutate(
      DayTime = seq(ymd_hm("2010-01-04 00:00"), ymd_hm("2010-01-10 23:50"), by = "10 mins")[1:n()],
      Type = type
    ) %>%
    rename(Time = DayTime)
}

# Filters data based on the specified type of day ("working day" or "weekend day").
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals for each day of the week).
# 'Demand' : Power demand for each timestamp.
# 'Type'   : Type of data ("Household", "L1 charging only", "L2 charging only").
# 'Day'    : Day of the week (abbreviated).
filter.by.daytype <- function(df, type) {
  df %>%
    mutate(
      Day = format(Time, "%a"),
      DayType = case_when(
        Day %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ "working day",
        Day %in% c("Sat", "Sun") ~ "weekend day"
      )
    ) %>%
    filter(DayType == type) %>%
    select(-DayType)
}

# Prepare Plots
# =============================================================================
plot1 <- function(df) {
  # Total power demand per month
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = Month, y = Demand, fill = Type)) + 
    geom_bar(stat = "identity", position = "dodge") +
    labs(y = "Total demand (W)") +
    myfill + mytheme
}

plot2 <- function(df) {
  # Average yearly power demand on working/weekend days
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4"))
  mycolor <- scale_color_manual(values = c("black", "black", "black"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Time, y = Demand)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = Demand, fill = Type), alpha = .6) +
    facet_grid(. ~ Type) +
    scale_x_datetime(date_labels = "%a") +
    labs(y = "Average demand (W)") +
    myfill + mycolor + mytheme
}

plot3 <- function(df) {
  # Average yearly power demand on working/weekend days (bar chart)
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Day, y = Demand, fill = Day)) + 
    geom_bar(stat = "identity", position = "dodge") +
    facet_grid(. ~ Type) +
    labs(y = "Average demand (W)") +
    myfill + mytheme
}

plot4 <- function(df) {
  # Average yearly power demand on working/weekend days (box plot)
  myfill <- scale_fill_manual(values = c("slateblue4", "orangered2", "seagreen4", "orchid3", "sienna2"))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "none")
  
  ggplot(df, aes(x = Day, y = Demand, fill = Day)) + 
    geom_boxplot() +
    facet_grid(. ~ Type) + 
    labs(y = "Average demand (W)") +
    myfill + mytheme
}

# Calculate power demand per month for household and PEV scenarios
# =============================================================================
House <- demand("House.csv") # Sum household demand
Total_L1 <- tot.demand(House, "PEV_L1.csv") # Sum household + PEV demand (L1)
Total_L2 <- tot.demand(House, "PEV_L2.csv") # Sum household + PEV demand (L2)

House_Month_SUM <- sum.month.demand(House, "House") # Monthly household demand
Total_L1_Month_SUM <- sum.month.demand(Total_L1, "House + L1") # Monthly household + PEV demand (L1)
Total_L2_Month_SUM <- sum.month.demand(Total_L2, "House + L2") # Monthly household + PEV demand (L2)

# Combine the data for all scenarios into a single data frame
Month_SUM <- rbind(Total_L2_Month_SUM, Total_L1_Month_SUM, House_Month_SUM)
Month_SUM$Type <- factor(Month_SUM$Type, levels = c("House + L2", "House + L1", "House"))
Month_SUM$Month <- factor(Month_SUM$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Calculate average power demand on working/weekend days for household and PEV scenarios
# =============================================================================
House <- avg.demand("House.csv", "Household only") # Avg household demand
PEV_L1 <- avg.demand("PEV_L1.csv", "L1 charging only") # Avg PEV demand (L1)
PEV_L2 <- avg.demand("PEV_L2.csv", "L2 charging only") # Avg PEV demand (L2)

# Combine the data for all scenarios into a single data frame
Year_AVG <- rbind(House, PEV_L1, PEV_L2)

# Filter the data by working days
Working_Day <- filter.by.daytype(Year_AVG, "working day")
Working_Day$Day <- factor(Working_Day$Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Filter the data by weekend days
Weekend_Day <- filter.by.daytype(Year_AVG, "weekend day")
Weekend_Day$Day <- factor(Weekend_Day$Day, levels = c("Sat", "Sun"))

# Display Plots
# =============================================================================
plot1(Month_SUM) # Total power demand per month

# ==
plot2(Working_Day) # Average yearly power demand on working days
plot3(Working_Day) # Average yearly power demand on working days (bar chart)
plot4(Working_Day) # Average yearly power demand on working days (box plot)

# ==
plot2(Weekend_Day) # Average yearly power demand on weekend days
plot3(Weekend_Day) # Average yearly power demand on weekend days (bar chart)
plot4(Weekend_Day) # Average yearly power demand on weekend days (box plot)
