# =============================================================================
# Script to Convert PEV_L1 and PEV_L2 Structure to Time Zones Structure
# =============================================================================
# This script creates the new Time Zones structure by processing the two CSV
# files containing PEV charging profiles (PEV_L1.csv and PEV_L2.csv). The 
# structure classifies power demand into four time zones: Shoulder 1, Peak, 
# Shoulder 2, and Off Peak.
#
# The output CSV file includes the following columns:
# 'Charge_Type'     : (Character) Type of charging level ("L1", "L2")
# 'Date'            : (Character) Date of charging (in d/m/yyyy format, e.g., "5/1/2010")
# 'HV_Code'         : (Character) PEV code (e.g., "H001.V001")
# 'Charge_Duration' : (Integer)   Duration of charging (in 10-minute time slots).
# 'Start_Time'      : (Character) Start time of charging session (in H:MM format).
# 'Stop_Time'       : (Character) Stop time of charging session (in H:MM format).
# 'Time_Zone'       : (Character) Classification of charging time into a zone (e.g., "Shoulder 1", "Peak").
# 'DayType'         : (Character) Indicates whether the day is a weekday ("W") or weekend ("H").
# 'KWh'             : (Numeric)   Energy consumed during charging session, in kilowatt-hours (kWh). 
# 'Splits'          : (Logical)   Indicates whether the charging session spans more than one time zone. 

# Load Packages
# =============================================================================
library(lubridate)
library(dplyr)

# Set Current Working Directory (adjust path as needed)
# =============================================================================
setwd("D:/Informatics/BigData") # Path to data files (CSV)

# Define Functions
# =============================================================================
# Generates a sequence of slots for time zones
create.time.slots <- function(start_time, stop_time) {
  start_time <- strptime(start_time, format = "%H:%M")
  stop_time <- strptime(stop_time, format = "%H:%M")
  
  slots <- seq(start_time, stop_time, by = "10 min")
  
  return(paste(hour(slots), format(slots,"%M"), sep = ":"))
}

# Checks if the charge should split based on time zone and day type
should.split.charge <- function(df, i, weekdayTimeZones, weekendTimeZones) {
  nextChargeTime <- paste(hour(df$Time[i+1]), format(df$Time[i+1], "%M"), sep = ":")
  
  # Check if the next charge time falls within the weekday or weekend time zone boundaries
  shouldSplitWeekday <- (df$DayType[i+1] == "W") && (nextChargeTime %in% weekdayTimeZones)
  shouldSplitWeekend <- (df$DayType[i+1] == "H") && (nextChargeTime %in% weekendTimeZones)
  
  return(shouldSplitWeekday | shouldSplitWeekend)
}

# Converts a PEV_L1 or PEV_L2 structure to Time Zones structure
convert.to.time.zones <- function(file, watt, type) {
  df <- read.csv2(file)
  df <- df %>%
    mutate(
      Time = dmy_hm(Time),
      DayType = case_when(
        format(Time, "%a") %in% c("Mon", "Tue", "Wed", "Thu", "Fri") ~ "W",  # Weekdays: "W"
        format(Time, "%a") %in% c("Sat", "Sun") ~ "H",  # Weekends: "H"
      )
    )
  
  # Preprocess date range selection (adjust timestamps as needed)
  lower <- which(df$Time >= dmy_hm("4-1-2010 0:00"))[1]
  upper <- tail(which(df$Time <= dmy_hm("8-1-2010 23:50")), 1)
  
  # Define time zone boundaries for weekdays and weekends (holidays)
  weekdayTimeZones <- c("7:00", "14:00", "20:00", "22:00")  # Weekday Time Zones: Shoulder 1, Peak, Shoulder 2, Off Peak
  weekendTimeZones <- c("7:00", "22:00")  # Weekend Time Zones: Shoulder, Off Peak
  
  # Initialize vectors for charging details
  HV_Code <- NA         # (Character): PEV code (e.g., "H001.V001").
  Date <- NA            # (Character): Date of charging (in d/m/yyyy format, e.g., "5/1/2010").
  Charge_Duration <- NA # (Integer)  : Duration of charging (in 10-minute time slots).
  Start_Time <- NA      # (Character): Start time of charging session (in H:MM format).
  Stop_Time <- NA       # (Character): Stop time of charging session (in H:MM format).
  DayType <- NA         # (Character): Indicates whether the day is a weekday ("W") or weekend ("H").
  
  # Initialize variables for loop control
  duration <- 0         # (Integer)  : Tracks the ongoing charging duration.
  k <- 1                # (Integer)  : Counter for indexing the vectors, starts at 1.
  
  for (j in 2:ncol(df)) { # Iterate over PEVs (columns)
    for (i in lower:upper) { # Iterate over time slots (rows)
      
      # First occurrence (start of a new charging session)
      if (df[i,j] == watt && duration == 0) {
        HV_Code[k] <- names(df)[j]
        Date[k] <- paste(day(df$Time[i]), month(df$Time[i]), year(df$Time[i]), sep = "/")
        Start_Time[k] <- paste(hour(df$Time[i]), format(df$Time[i],"%M"), sep = ":")
        DayType[k] <- df$DayType[i]
        
        duration <- 1
        
        # If the charge should split (i.e., the next time falls into a new time zone), update charge details
        if (should.split.charge(df, i, weekdayTimeZones, weekendTimeZones)) {
          Charge_Duration[k] <- duration
          Stop_Time[k] <- paste(hour(df$Time[i]), format(df$Time[i],"%M"), sep = ":")
          
          # Reset for next charge
          duration <- 0
          k <- k + 1
        }
      }
      
      # Next occurrences (continuing charging)
      else if (df[i,j] == watt && duration != 0) {
        duration <- duration + 1
        
        # If the charge should split (i.e., the next time falls into a new time zone), update charge details
        if (should.split.charge(df, i, weekdayTimeZones, weekendTimeZones)) {
          Charge_Duration[k] <- duration
          Stop_Time[k] <- paste(hour(df$Time[i]), format(df$Time[i],"%M"), sep = ":")
          
          # Reset for next charge
          duration <- 0
          k <- k + 1
        }
      }
      
      # Last occurrence (end of charging session)
      else if (df[i,j] == 0 && duration != 0) {
        Charge_Duration[k] <- duration
        Stop_Time[k] <- paste(hour(df$Time[i-1]), format(df$Time[i-1],"%M"), sep = ":")
        
        # Reset for next charge
        duration <- 0
        k <- k + 1
      }
    }
  }
  
  Charge_Type <- rep(type, k-1)
  KWh <- (Charge_Duration/6) * (watt/1000)
  
  TZ_LX <- data.frame(Charge_Type, Date, HV_Code, Charge_Duration, Start_Time, Stop_Time, DayType, KWh, stringsAsFactors = FALSE)
  return(TZ_LX)
}

# Convert PEV_L1 and PEV_L2 structures to Time Zones structure
# =============================================================================
TZ_L1 <- convert.to.time.zones("PEV_L1.csv", 1920, "L1")
TZ_L2 <- convert.to.time.zones("PEV_L2.csv", 6600, "L2")

TZ <- rbind (TZ_L1, TZ_L2)
TZ$Time_Zone <- NA
TZ <- TZ[, c(colnames(TZ)[1:6], "Time_Zone", "DayType", "KWh")]

# Check for charge splits based on stop and start times crossing time zones
# =============================================================================
TZ$Splits <- FALSE

beforeSplitTimes <- c("6:50", "13:50", "19:50", "21:50")
splitTimes <- c("7:00", "14:00", "20:00", "22:00")

for(i in 2:nrow(TZ)) {
  if (TZ$Date[i] == TZ$Date[i-1]) {
    if (TZ$Stop_Time[i-1] %in% beforeSplitTimes & TZ$Start_Time[i] %in% splitTimes) {
      TZ$Splits[i-1] <- TRUE
      TZ$Splits[i] <- TRUE
    }
  }
}

# Calculate and print the percentage of splits for L1 and L2 charge types
sum(TZ$Splits[TZ$Charge_Type == "L1"]) / sum(TZ$Charge_Type == "L1") * 100
sum(TZ$Splits[TZ$Charge_Type == "L2"]) / sum(TZ$Charge_Type == "L2") * 100

# Create time zone slots and fill Time_Zone column
# =============================================================================
weekdayShoulder1 <- create.time.slots("7:00", "13:50")
weekdayPeak <- create.time.slots("14:00", "19:50")
weekdayShoulder2 <- create.time.slots("20:00", "21:50")
# ==
weekendShoulder <- create.time.slots("7:00", "21:50")

TZ <- TZ %>%
  mutate(
    Time_Zone = case_when(
      # Weekdays (W)
      DayType == "W" & Start_Time %in% weekdayShoulder1 ~ "Shoulder 1",
      DayType == "W" & Start_Time %in% weekdayPeak ~ "Peak",
      DayType == "W" & Start_Time %in% weekdayShoulder2 ~ "Shoulder 2",
      DayType == "W" ~ "Off Peak",
      
      # Weekends (H)
      DayType == "H" & Start_Time %in% weekendShoulder ~ "Shoulder",
      DayType == "H" ~ "Off Peak"
    )
  )

# Export Time Zones Data Frame to CSV Format (adjust path as needed)
# =============================================================================
write.csv2(TZ, "TimeZones.csv", row.names = FALSE) # Path to export file
