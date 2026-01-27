# =============================================================================
# Script to Convert Time Zones or Load Shifting Structure to PEV_L1 and PEV_L2 Structure
# =============================================================================
# This script takes a Time Zones or Load Shifting structure (which classifies
# PEV power demand into four time zones) and converts it back to the original 
# PEV_L1 and PEV_L2 charging profiles.
#
# The output CSV files (one for L1 and one for L2) include the following columns:
# 'Time'        : Represents the 10-minute intervals (in "d/m/yyyy H:MM" format).
# 'PEV Columns' : Each column represents a PEV, with power demand recorded for 
#                 each 10-minute interval. Zero values indicate no charging.

# Load Packages
# =============================================================================
library(lubridate)
library(dplyr)
library(glue)

# Set Current Working Directory (adjust path as needed)
# =============================================================================
setwd("../data") # Path to the /data directory

# Define Functions
# =============================================================================
# Converts a Time Zones or Load Shifting structure to PEV_L1 and PEV_L2 structures
convert.to.pev_lx <- function(pev_df, tz_df, watt) {
  charge_progress <- 1 # Tracks the progress of each charging session
  
  # Set k index to the current charging session (L1 or L2)
  if (watt == 6600) 
    k <- match("L2", tz_df$Charge_Type) # Find the index of the "L2" charge type
  else 
    k <- 1 # Default to the first session for L1
  
  for (j in 2:ncol(pev_df)) { # Iterate over PEVs (columns)
    for (i in 1:nrow(pev_df)) { # Iterate over time slots (rows)
      DateTime <- paste(tz_df$Date[k], tz_df$Start_Time[k])
      
      # First occurrence (start of a new charging session)
      if (pev_df$Time[i] == DateTime) {
        names(pev_df)[j] <- tz_df$HV_Code[k]
        pev_df[i,j] <- watt
        charge_progress <- charge_progress + 1
        
        # If Charge Duration is 1, move to the next session
        if (tz_df$Charge_Duration[k] == 1) {
          # Reset for next charge
          charge_progress <- 1
          k <- k + 1
        }
      }
      
      # Next occurrences (continuing charging)
      else if (charge_progress > 1) {
        pev_df[i,j] <- watt
        
        # Check if last occurrence (end of charging session)
        if (charge_progress == tz_df$Charge_Duration[k]) {
          # Reset for next charge
          charge_progress <- 1
          k <- k + 1
        } else {
          charge_progress <- charge_progress + 1
        }
      }
    }
  }
  return(pev_df)
}

# Read Time Zones or Load Shifting Data Frame (adjust path as needed)
# =============================================================================
TZ <- read.csv2(file.path("time_zones", "TimeZones.csv"), stringsAsFactors = FALSE) # Path to CSV file
# Check if 'Noise' column exists before filtering
if ("Noise" %in% colnames(TZ)) {
  TZ <- filter(TZ, Noise == 0)  # Filter out noisy data
} else {
  message("'Noise' column is missing, skipping filtering.")
}

# Prepare Empty PEV_L1 and PEV_L2 Data Frames
# =============================================================================
# Generate time sequence (adjust timestamps as needed)
Time <- seq(dmy_hm("4-1-2010 00:00"), dmy_hm("8-1-2010 23:50"), by = "10 mins")
Time <- glue("{day(Time)}/{month(Time)}/{year(Time)} {hour(Time)}:{format(Time, '%M')}")

# Create the Data Frames with 348 columns for PEVs
PEV_L1 <- data.frame(Time = Time, matrix(0, ncol = 348, nrow = length(Time)))
PEV_L2 <- data.frame(Time = Time, matrix(0, ncol = 348, nrow = length(Time)))

# Convert Time Zones or Load Shifting structure to PEV_L1 and PEV_L2 structures
# =============================================================================
PEV_L1 <- convert.to.pev_lx(PEV_L1, TZ, 1920)
PEV_L2 <- convert.to.pev_lx(PEV_L2, TZ, 6600)

# Export PEV_L1 and PEV_L2 Data Frames to CSV Format (adjust path as needed)
# =============================================================================
write.csv2(PEV_L1, "PEV_L1_01.csv", row.names = FALSE) # Path to export PEV_L1 file
write.csv2(PEV_L2, "PEV_L2_01.csv", row.names = FALSE) # Path to export PEV_L2 file
