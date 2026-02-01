# =============================================================================
# Script to Generate the Plots of Chapter 6 Studying Load Shifting Cases
# =============================================================================
# This script presents the results of load shifting applied to the Time Zones
# structure across two main cases, each containing five subcases. The simulation 
# used varying percentages of load shifted between time zones for each subcase.
#
#  - Case 1: Load shifting using the same percentage for a1, b1, and c1 in each subcase:
#    - Subcase 1: 0.1 (10% of the load)
#    - Subcase 2: 0.2 (20% of the load)
#    - Subcase 3: 0.3 (30% of the load)
#    - Subcase 4: 0.4 (40% of the load)
#    - Subcase 5: 0.5 (50% of the load)
#
#  - Case 2: Load shifting using only the a1 percentage across all subcases:
#    - Subcase 1: 0.1 (10% of the load)
#    - Subcase 2: 0.2 (20% of the load)
#    - Subcase 3: 0.3 (30% of the load)
#    - Subcase 4: 0.4 (40% of the load)
#    - Subcase 5: 0.5 (50% of the load)
#
# Several visualizations were created to study each case and subcase, including
# kWh per time zone, weekly and daily power demand, and energy saving comparisons.

# Set the case number (1 or 2) and the corresponding percentage setting
case <- 1                  # Set "1" for Case 1 or "2" for Case 2
percentage <- "a1, b1, c1" # Set "a1, b1, c1" for Case 1 or "a1" for Case 2

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
# Sums the kWh of all PEVs by time zone for L1 charges on Tuesday.
#
# Returns a data frame with the following columns:
# 'Time_Zone' : The time zone (e.g., Peak, Off-Peak, etc.).
# 'KWh'       : Total kWh for the specified time zone.
# 'a1b1c1'    : The percentage parameter passed to the function.
kwh.by.zone <- function(file, percentage) {
  df <- read.csv2(file, stringsAsFactors = FALSE)
  df %>%
    mutate(
      Charge_Date = dmy(Charge_Date), 
      Day = format(Charge_Date, "%a")
      ) %>%
    filter(Charge_Type == "L1" & Day == "Tue") %>%
    group_by(Time_Zone) %>% 
    summarise(KWh = sum(KWh), .groups = "drop") %>% 
    mutate(a1b1c1 = percentage) 
}

# Sums the power demand of all PEVs for each timestamp for the load-shifting week.
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals for the load-shifting week).
# 'Demand' : Total power demand for each timestamp.
# 'Type'   : Type of data ("Before Load Shifting", "After Load Shifting").
demand <- function(file, type) {
  df <- read.csv2(file)
  
  df %>%
    mutate(
      Demand = rowSums(select(., -Time)), 
      Time = dmy_hm(Time), 
      Type = type
      ) %>%
    filter(Time >= ymd_hm("2010-01-04 00:00") & Time <= ymd_hm("2010-01-08 23:50")) %>%
    select(Time, Demand, Type)
}

# Sums the power demand of all PEVs for each timestamp on Tuesday.
#
# Returns a data frame with the following columns:
# 'Time'   : Timestamp (in 10-minute intervals on Tuesday).
# 'Demand' : Total power demand for each timestamp.
# 'Type'   : Type of data ("Before Load Shifting", "After Load Shifting").
tue.demand <- function(file, type) {
  df <- read.csv2(file)
  
  df %>%
    mutate(
      Demand = rowSums(select(., -Time)), 
      Time = dmy_hm(Time),
      Type = type
      ) %>%
    filter(Time >= ymd_hm("2010-01-04 22:00") & Time <= ymd_hm("2010-01-06 6:50")) %>%
    select(Time, Demand, Type)
}

# Prepare Plots
# =============================================================================
plot1 <- function(df, type) {
  # Case X: kWh per time zone for L1 charges on Tuesday"
  dark2_colors <- brewer.pal(7, "Dark2")
  
  mycolor <- scale_color_manual(values = c(dark2_colors[3], dark2_colors[2], dark2_colors[7], dark2_colors[6]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_text(face = "bold"),
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = a1b1c1, y = KWh, color = Time_Zone, size = 4)) + geom_point() +
    labs(color = "Time Zone", x = type) +
    guides(colour = guide_legend(override.aes = list(size = 4))) +
    scale_size(guide = "none") +
    mycolor + mytheme
}

plot2 <- function(df) {
  # Case X - Subcase Y: Weekly power demand with L1/L2 charging
  dates <- seq(dmy("4/1/2010"), dmy("8/1/2010"), by = "1 day")
  hours <- c("7:00", "14:00", "20:00", "22:00")
  date_str <- format(dates, "%d/%m/%Y")
  dark2_colors <- brewer.pal(3, "Dark2")
  
  mytimes <- dmy_hm(paste(rep(date_str, each = length(hours)), hours))
  myfill <- scale_fill_manual(values = c(dark2_colors[3], dark2_colors[2]))
  mycolor <- scale_color_manual(values = c("black", "black", "black"))
  mytheme <- theme(plot.title = element_text(hjust = 0.3),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = Time, y = Demand, color = Type)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = Demand, fill = Type), alpha = .6) +
    geom_vline(xintercept = mytimes, linetype = "dashed", linewidth = 1) +
    scale_x_datetime(date_labels = "%a", breaks = "1 day") +
    labs(y = "Power Demand (W)") +
    myfill + mycolor + mytheme
}

plot3 <- function(df) {
  # Case X - Subcase Y: Daily power demand with L1/L2 charging
  dark2_colors <- brewer.pal(3, "Dark2")
  
  mytimes <- c(dmy_hm("4/1/2010 22:00"), dmy_hm("5/1/2010 7:00"), dmy_hm("5/1/2010 14:00"), dmy_hm("5/1/2010 20:00"), dmy_hm("5/1/2010 22:00"), dmy_hm("6/1/2010 7:00"))
  mycolor <- scale_color_manual(values = c(dark2_colors[3], dark2_colors[2]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   axis.title.x = element_blank(), 
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = Time, y = Demand, color = Type)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = mytimes, linetype = "dashed", linewidth = 1) +
    scale_x_datetime(date_labels = "%H:%M", breaks = "4 hours") +
    labs(y = "Power Demand (W)") +
    mycolor + mytheme
}

plot4 <- function(df, type) {
  # Energy saving for Case X
  dark2_colors <- brewer.pal(3, "Dark2")
  
  mycolor <- scale_color_manual(values = c(dark2_colors[2], dark2_colors[3]))
  mytheme <- theme(plot.title = element_text(hjust = 0.5),
                   legend.position = "top", 
                   legend.justification = "left",
                   legend.direction = "horizontal",
                   legend.title = element_blank(), 
                   legend.background = element_rect(colour = "black"))
  
  ggplot(df, aes(x = a1b1c1, y = Percentage, group = Type)) +
    geom_line(aes(color = Type), linewidth = 1) +
    facet_grid(. ~ Class) +
    labs(x = type) +
    mycolor + mytheme
}

# Calculate kWh by time zone for L1 charges on Tuesday across all subcases
# =============================================================================
TZ <- kwh.by.zone(file.path("time_zones", "TimeZones.csv"), 0)
LS01 <- kwh.by.zone(file.path("load_shifting", paste0("case", case), "LoadShifting01.csv"), 0.1)
LS02 <- kwh.by.zone(file.path("load_shifting", paste0("case", case), "LoadShifting02.csv"), 0.2)
LS03 <- kwh.by.zone(file.path("load_shifting", paste0("case", case), "LoadShifting03.csv"), 0.3)
LS04 <- kwh.by.zone(file.path("load_shifting", paste0("case", case), "LoadShifting04.csv"), 0.4)
LS05 <- kwh.by.zone(file.path("load_shifting", paste0("case", case), "LoadShifting05.csv"), 0.5)

LS <- rbind(TZ, LS01, LS02, LS03, LS04, LS05)

# Calculate weekly power demand for all subcases
# =============================================================================
PEV_L1_00 <- demand(file.path("processed", "PEV_L1.csv"), "Before Load Shifting")
PEV_L2_00 <- demand(file.path("processed", "PEV_L2.csv"), "Before Load Shifting")

PEV_L1_01 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L1_01.csv"), "After Load Shifting")
PEV_L1_01 <- rbind(PEV_L1_00, PEV_L1_01)
PEV_L1_01$Type <- factor(PEV_L1_01$Type, levels = c("Before Load Shifting", "After Load Shifting"))
PEV_L2_01 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L2_01.csv"), "After Load Shifting")
PEV_L2_01 <- rbind(PEV_L2_00, PEV_L2_01)
PEV_L2_01$Type <- factor(PEV_L2_01$Type, levels = c("Before Load Shifting", "After Load Shifting"))

PEV_L1_02 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L1_02.csv"), "After Load Shifting")
PEV_L1_02 <- rbind(PEV_L1_00, PEV_L1_02)
PEV_L1_02$Type <- factor(PEV_L1_02$Type, levels = c("Before Load Shifting", "After Load Shifting"))
PEV_L2_02 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L2_02.csv"), "After Load Shifting")
PEV_L2_02 <- rbind(PEV_L2_00, PEV_L2_02)
PEV_L2_02$Type <- factor(PEV_L2_02$Type, levels = c("Before Load Shifting", "After Load Shifting"))

PEV_L1_03 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L1_03.csv"), "After Load Shifting")
PEV_L1_03 <- rbind(PEV_L1_00, PEV_L1_03)
PEV_L1_03$Type <- factor(PEV_L1_03$Type, levels = c("Before Load Shifting", "After Load Shifting"))
PEV_L2_03 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L2_03.csv"), "After Load Shifting")
PEV_L2_03 <- rbind(PEV_L2_00, PEV_L2_03)
PEV_L2_03$Type <- factor(PEV_L2_03$Type, levels = c("Before Load Shifting", "After Load Shifting"))

PEV_L1_04 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L1_04.csv"), "After Load Shifting")
PEV_L1_04 <- rbind(PEV_L1_00, PEV_L1_04)
PEV_L1_04$Type <- factor(PEV_L1_04$Type, levels = c("Before Load Shifting", "After Load Shifting"))
PEV_L2_04 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L2_04.csv"), "After Load Shifting")
PEV_L2_04 <- rbind(PEV_L2_00, PEV_L2_04)
PEV_L2_04$Type <- factor(PEV_L2_04$Type, levels = c("Before Load Shifting", "After Load Shifting"))

PEV_L1_05 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L1_05.csv"), "After Load Shifting")
PEV_L1_05 <- rbind(PEV_L1_00, PEV_L1_05)
PEV_L1_05$Type <- factor(PEV_L1_05$Type, levels = c("Before Load Shifting", "After Load Shifting"))
PEV_L2_05 <- demand(file.path("load_shifting", paste0("case", case), "PEV_L2_05.csv"), "After Load Shifting")
PEV_L2_05 <- rbind(PEV_L2_00, PEV_L2_05)
PEV_L2_05$Type <- factor(PEV_L2_05$Type, levels = c("Before Load Shifting", "After Load Shifting"))

# Calculate total power demand on Tuesday for all subcases
# =============================================================================
Tue_PEV_L1_00 <- tue.demand(file.path("processed", "PEV_L1.csv"), "Before Load Shifting")
Tue_PEV_L2_00 <- tue.demand(file.path("processed", "PEV_L2.csv"), "Before Load Shifting")

Tue_PEV_L1_01 <- tue.demand(file.path("load_shifting", paste0("case", case), "PEV_L1_01.csv"), "After Load Shifting")
Tue_PEV_L1_01 <- rbind(Tue_PEV_L1_00, Tue_PEV_L1_01)
Tue_PEV_L1_01$Type <- factor(Tue_PEV_L1_01$Type, levels = c("Before Load Shifting", "After Load Shifting"))
Tue_PEV_L2_01 <- tue.demand(file.path("load_shifting", paste0("case", case), "PEV_L2_01.csv"), "After Load Shifting")
Tue_PEV_L2_01 <- rbind(Tue_PEV_L2_00, Tue_PEV_L2_01)
Tue_PEV_L2_01$Type <- factor(Tue_PEV_L2_01$Type, levels = c("Before Load Shifting", "After Load Shifting"))

Tue_PEV_L1_03 <- tue.demand(file.path("load_shifting", paste0("case", case), "PEV_L1_03.csv"), "After Load Shifting")
Tue_PEV_L1_03 <- rbind(Tue_PEV_L1_00, Tue_PEV_L1_03)
Tue_PEV_L1_03$Type <- factor(Tue_PEV_L1_03$Type, levels = c("Before Load Shifting", "After Load Shifting"))
Tue_PEV_L2_03 <- tue.demand(file.path("load_shifting", paste0("case", case), "PEV_L2_03.csv"), "After Load Shifting")
Tue_PEV_L2_03 <- rbind(Tue_PEV_L2_00, Tue_PEV_L2_03)
Tue_PEV_L2_03$Type <- factor(Tue_PEV_L2_03$Type, levels = c("Before Load Shifting", "After Load Shifting"))

Tue_PEV_L1_05 <- tue.demand(file.path("load_shifting", paste0("case", case), "PEV_L1_05.csv"), "After Load Shifting")
Tue_PEV_L1_05 <- rbind(Tue_PEV_L1_00, Tue_PEV_L1_05)
Tue_PEV_L1_05$Type <- factor(Tue_PEV_L1_05$Type, levels = c("Before Load Shifting", "After Load Shifting"))
Tue_PEV_L2_05 <- tue.demand(file.path("load_shifting", paste0("case", case), "PEV_L2_05.csv"), "After Load Shifting")
Tue_PEV_L2_05 <- rbind(Tue_PEV_L2_00, Tue_PEV_L2_05)
Tue_PEV_L2_05$Type <- factor(Tue_PEV_L2_05$Type, levels = c("Before Load Shifting", "After Load Shifting"))

# Import Enegry Saving Data
# =============================================================================
ES <- read.csv2(file.path("load_shifting", paste0("case", case), "EnergySaving.csv"), dec = ".")

# Display Plots
# =============================================================================
plot1(LS, percentage) # Case X: kWh per time zone for L1 charges on Tuesday

# ==
plot2(PEV_L1_01) # Case X - Subcase 1: Weekly power demand with L1 charging
plot2(PEV_L1_02) # Case X - Subcase 2: Weekly power demand with L1 charging
plot2(PEV_L1_03) # Case X - Subcase 3: Weekly power demand with L1 charging
plot2(PEV_L1_04) # Case X - Subcase 4: Weekly power demand with L1 charging
plot2(PEV_L1_05) # Case X - Subcase 5: Weekly power demand with L1 charging

plot2(PEV_L2_01) # Case X - Subcase 1: Weekly power demand with L2 charging
plot2(PEV_L2_02) # Case X - Subcase 2: Weekly power demand with L2 charging
plot2(PEV_L2_03) # Case X - Subcase 3: Weekly power demand with L2 charging
plot2(PEV_L2_04) # Case X - Subcase 4: Weekly power demand with L2 charging
plot2(PEV_L2_05) # Case X - Subcase 5: Weekly power demand with L2 charging

# ==
plot3(Tue_PEV_L1_01) # Case X - Subcase 1: Daily power demand with L1 charging
plot3(Tue_PEV_L1_03) # Case X - Subcase 3: Daily power demand with L1 charging
plot3(Tue_PEV_L1_05) # Case X - Subcase 5: Daily power demand with L1 charging

plot3(Tue_PEV_L2_01) # Case X - Subcase 1: Daily power demand with L2 charging
plot3(Tue_PEV_L2_03) # Case X - Subcase 3: Daily power demand with L2 charging
plot3(Tue_PEV_L2_05) # Case X - Subcase 5: Daily power demand with L2 charging

# ==
plot4(ES, percentage) # Energy saving for Case X
