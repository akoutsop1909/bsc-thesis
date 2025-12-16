# =============================================================================
# Script to Perform Load Shifting on a Time Zones Structure
# =============================================================================
# This script performs load shifting on a Time Zones structure, with adjustable
# parameters (a1, b1, c1) that control the percentage of load shifted between 
# different time zones. The model attempts up to a specified number of times to 
# place a migrated charge into an available slot in the target zone. If no valid 
# slot is found, the 'Noise' column records the percentage of overlapping kWh 
# with other charges from the same PEV.

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
  
  # Check if stop_time is earlier than start_time (indicating it spans to the next day)
  if (stop_time < start_time) {
    stop_time <- stop_time + 24*60*60  # Add 24 hours to stop_time to shift to the next day
  }
  
  slots <- seq(start_time, stop_time, by = "10 min")
  
  return(paste(hour(slots), format(slots,"%M"), sep = ":"))
}

# Samples from peak and redistributes the load to off peak, shoulder 1, and shoulder 2
peak.sample.shifting <- function(df, dailyIndexes) {
  # Initialize index vectors for the different time zones
  peakIndexes <- c()
  shoulder1Indexes <- c()
  shoulder2Indexes <- c()
  offPeakIndexes <- c()
  
  # Filter dailyIndexes to ensure it only contains indexes from peak
  dailyIndexes <- dailyIndexes[df$Time_Zone[dailyIndexes] == "Peak"]
  
  # Filter the data for the current day's entries
  sampleData <- filter(df, Index %in% dailyIndexes)
  totalKWH <- sum(sampleData$KWh) # Total kWh for this day
  
  # Sample indexes from peak
  sampleKWH <- 0
  kWhPercentage <- 0
  while (kWhPercentage < a1 && length(dailyIndexes) != 0) {
    sampleIndex <- sample(dailyIndexes, size = 1)
    peakIndexes <- c(peakIndexes, sampleIndex)
    dailyIndexes <- dailyIndexes[dailyIndexes != sampleIndex]
    
    sampleKWH <- sampleKWH + df$KWh[sampleIndex]
    kWhPercentage <- sampleKWH / totalKWH
  }
  
  # Filter data based on sampled peak indexes
  sampleData <- filter(df, Index %in% peakIndexes)
  totalmKWH <- sum(sampleData$KWh) # Total kWh for sampled peak indexes
  
  # Sample indexes for off peak from sampled peak indexes
  sampleKWH <- 0
  kWhPercentage <- 0
  while (kWhPercentage < d1 && length(peakIndexes) != 0) {
    sampleIndex <- sample(peakIndexes, size = 1)
    offPeakIndexes <- c(offPeakIndexes, sampleIndex)
    peakIndexes <- peakIndexes[peakIndexes != sampleIndex]
    
    sampleKWH <- sampleKWH + df$KWh[sampleIndex]
    kWhPercentage <- sampleKWH / totalKWH
  }
  
  # Sample indexes for shoulder 1 from remaining sampled peak indexes
  sampleKWH <- 0
  kWhPercentage <- 0
  while (kWhPercentage < e1 && length(peakIndexes) != 0) {
    sampleIndex <- sample(peakIndexes, size = 1)
    shoulder1Indexes <- c(shoulder1Indexes, sampleIndex)
    peakIndexes <- peakIndexes[peakIndexes != sampleIndex]
    
    sampleKWH <- sampleKWH + df$KWh[sampleIndex]
    kWhPercentage <- sampleKWH / totalKWH
  }
  
  # The remaining sampled peak indexes are allocated to shoulder 2
  shoulder2Indexes <- peakIndexes
  
  # Perform load shifting for each group
  for (i in 1:length(offPeakIndexes)) df <- load.shifting(df, offPeakIndexes[i], "Off Peak")
  for (i in 1:length(shoulder1Indexes)) df <- load.shifting(df, shoulder1Indexes[i], "Shoulder 1")
  for (i in 1:length(shoulder2Indexes)) df <- load.shifting(df, shoulder2Indexes[i], "Shoulder 2")
  
  return(df)
}

# Samples from shoulder 1 and redistributes the load to off peak and shoulder 2
shoulder1.sample.shifting <- function(df, dailyIndexes) {
  # Initialize index vectors for the different time zones
  shoulder1Indexes <- c()
  shoulder2Indexes <- c()
  offPeakIndexes <- c()
  
  # Filter dailyIndexes to ensure it only contains indexes from shoulder 1
  dailyIndexes <- dailyIndexes[df$Time_Zone[dailyIndexes] == "Shoulder 1"]
  
  # Filter the data for the current day's entries
  sampleData <- filter(df, Index %in% dailyIndexes)
  totalKWH <- sum(sampleData$KWh) # Total kWh for this day
  
  # Sample indexes from shoulder 1
  sampleKWH <- 0
  kWhPercentage <- 0
  while (kWhPercentage < b1 && length(dailyIndexes) != 0) {
    sampleIndex <- sample(dailyIndexes, size = 1)
    shoulder1Indexes <- c(shoulder1Indexes, sampleIndex)
    dailyIndexes <- dailyIndexes[dailyIndexes != sampleIndex]
    
    sampleKWH <- sampleKWH + df$KWh[sampleIndex]
    kWhPercentage <- sampleKWH / totalKWH
  }
  
  # Filter data based on sampled shoulder 1 indexes
  sampleData <- filter(df, Index %in% shoulder1Indexes)
  totalmKWH <- sum(sampleData$KWh) # Total kWh for sampled shoulder 1 indexes
  
  # Sample indexes for off peak from sampled shoulder 1 indexes
  sampleKWH <- 0
  kWhPercentage <- 0
  while (kWhPercentage < g1 && length(shoulder1Indexes) != 0) {
    sampleIndex <- sample(shoulder1Indexes, size = 1)
    offPeakIndexes <- c(offPeakIndexes, sampleIndex)
    shoulder1Indexes <- shoulder1Indexes[shoulder1Indexes != sampleIndex]
    
    sampleKWH <- sampleKWH + df$KWh[sampleIndex]
    kWhPercentage <- sampleKWH / totalKWH
  }
  
  # The remaining sampled shoulder 1 indexes are allocated to shoulder 2
  shoulder2Indexes <- shoulder1Indexes
  
  # Perform load shifting for each group
  for (i in 1:length(offPeakIndexes)) df <- load.shifting(df, offPeakIndexes[i], "Off Peak")
  for (i in 1:length(shoulder2Indexes)) df <- load.shifting(df, shoulder2Indexes[i], "Shoulder 2")
  
  return(df)
}

# Samples from shoulder 2 and redistributes the load to off peak
shoulder2.sample.shifting <- function(df, dailyIndexes) {
  # Initialize index vectors for the different time zones
  shoulder2Indexes <- c()
  offPeakIndexes <- c()
  
  # Filter dailyIndexes to ensure it only contains indexes from shoulder 2
  dailyIndexes <- dailyIndexes[df$Time_Zone[dailyIndexes] == "Shoulder 2"]
  
  # Filter the data for the current day's entries
  sampleData <- filter(df, Index %in% dailyIndexes)
  totalKWH <- sum(sampleData$KWh) # Total kWh for this day
  
  # Sample indexes from shoulder 2
  sampleKWH <- 0
  kWhPercentage <- 0
  while (kWhPercentage < c1 && length(dailyIndexes) != 0) {
    sampleIndex <- sample(dailyIndexes, size = 1)
    shoulder2Indexes <- c(shoulder2Indexes, sampleIndex)
    dailyIndexes <- dailyIndexes[dailyIndexes != sampleIndex]
    
    sampleKWH <- sampleKWH + df$KWh[sampleIndex]
    kWhPercentage <- sampleKWH / totalKWH
  }
  
  # The sampled shoulder 2 indexes are allocated to off peak
  offPeakIndexes <- shoulder2Indexes
  
  # Perform load shifting for each group
  for (i in 1:length(offPeakIndexes)) df <- load.shifting(df, offPeakIndexes[i], "Off Peak")
  
  return(df)
}

# Migrates a charge to the target zone
load.shifting <- function(df, chargeIndex, targetZone) {
  # Exit if chargeIndex is empty, NULL, NA, or out of bounds
  if (is.null(chargeIndex) || length(chargeIndex) == 0 || is.na(chargeIndex) || chargeIndex < 1 || chargeIndex > nrow(df)) {
    return(df)
  }
  
  # Exit early if the original time zone is already off peak
  if (df$Time_Zone[chargeIndex] == "Off Peak") return(df)
  
  df$Time_Zone[chargeIndex] <- targetZone # Set the time zone
  
  # Filter same PEV charges in the target zone
  samePEVCharges <- filter(df, Index != chargeIndex & Charge_Type == df$Charge_Type[chargeIndex] & Date == df$Date[chargeIndex] & HV_Code == df$HV_Code[chargeIndex] & Time_Zone == df$Time_Zone[chargeIndex])
  
  # Determine target slots based on the target zone
  if (targetZone == "Shoulder 1") targetSlots <- shoulder1Slots
  else if (targetZone == "Shoulder 2") targetSlots <- shoulder2Slots
  else targetSlots <- offPeakSlots
  
  # Check if the charge doesn't fit into the target zone (can only happen in shoulder 2)
  if (targetZone == "Shoulder 2" && df$Charge_Duration[chargeIndex] > length(targetSlots)) {
    df$Start_Time[chargeIndex] <- targetSlots[1] # Set Start Time to the first slot
    df$Stop_Time[chargeIndex] <- targetSlots[length(targetSlots)] # Set Stop Time to the last slot
    df$Splits[chargeIndex] <- TRUE # The charge will split
    
    # Calculate the remaining duration that doesn't fit into the target zone (shoulder 2)
    overflowDuration <- df$Charge_Duration[chargeIndex] - length(targetSlots)
    df$Charge_Duration[chargeIndex] <- length(targetSlots) # Adjust duration to fit within the available slots
    
    # Calculate kWh for the adjusted duration and the overflow duration
    if (df$Charge_Type[chargeIndex] == "L1") {
      df$KWh[chargeIndex] <- (df$Charge_Duration[chargeIndex] / 6) * 1.92
      kwh <- (overflowDuration/6) * 1.92
    }
    else {
      df$KWh[chargeIndex] <- (df$Charge_Duration[chargeIndex] / 6) * 6.6
      kwh <- (overflowDuration/6) * 6.6
    }
    
    # Check for overlap with same PEV charges in the target zone (shoulder 2)
    df$Noise[chargeIndex] <- overlap(samePEVCharges, df[chargeIndex, ], targetSlots)
    
    # Add new row for the overflow charge
    newRowIndex <- max(df$Index) + 1 # Set the index for the new row
    df <- add_row(df, Index = newRowIndex, Charge_Type = df$Charge_Type[chargeIndex], 
                  Date = df$Date[chargeIndex], HV_Code = df$HV_Code[chargeIndex], 
                  Charge_Duration = overflowDuration, Start_Time = offPeakSlots[1],
                  Stop_Time = offPeakSlots[overflowDuration], Time_Zone = "Off Peak", 
                  DayType = "W", Splits = TRUE, KWh = kwh)
    
    # Filter same PEV charges in the target zone (off peak)
    samePEVCharges <- filter(df, Index != newRowIndex & Charge_Type == df$Charge_Type[newRowIndex] & Date == df$Date[newRowIndex] & HV_Code == df$HV_Code[newRowIndex] & Time_Zone == df$Time_Zone[newRowIndex])
    
    # Check for overlap with same PEV charges in the target zone (off peak)
    df$Noise[newRowIndex] <- overlap(samePEVCharges, df[newRowIndex, ], offPeakSlots)
    df$Noise[newRowIndex] <- df$Noise[newRowIndex] + spans.midnight.overlap(df, df[newRowIndex, ], offPeakSlots)
  }
  else {
    try <- 0
    slotFound <- FALSE
    maxTries <- 100 # (Adjust as needed) maximum number of tries to find a valid slot
    
    # Precompute the available range for random slot index
    availableSlots <- 1:(length(targetSlots) - df$Charge_Duration[chargeIndex] + 1)
    
    # Loop to find an available time slot for the charge within the target zone
    while (try < maxTries & !slotFound) {
      try <- try + 1
      
      # Generate a random slot index from the available slots and assign Start and Stop Time
      randomSlotIndex <- sample(availableSlots, 1)
      df$Start_Time[chargeIndex] <- targetSlots[randomSlotIndex]
      df$Stop_Time[chargeIndex] <- targetSlots[randomSlotIndex + df$Charge_Duration[chargeIndex] - 1]
      
      # Check for overlap with same PEV charges in the target zone
      df$Noise[chargeIndex] <- overlap(samePEVCharges, df[chargeIndex, ], targetSlots)
      if(targetZone == "Off Peak") df$Noise[chargeIndex] <- df$Noise[chargeIndex] + spans.midnight.overlap(df, df[chargeIndex, ], targetSlots)
      
      # If there is no overlap, exit the loop
      if (df$Noise[chargeIndex] == 0) slotFound <- TRUE
    }
  }
  return(df)
}

# Checks for overlap between the migrated charge and other charges of the same PEV in the target zone
overlap <- function(samePEVCharges, migratedCharge, targetSlots) {
  # If there are no charges of the same PEV in the target zone, return 0 (no overlap)
  if (nrow(samePEVCharges) == 0) return (0)
  
  # Find the position of the migrated charge in the targetSlots
  startIndex <- match(migratedCharge$Start_Time, targetSlots)
  stopIndex <- match(migratedCharge$Stop_Time, targetSlots)
  migratedChargeSlots <- targetSlots[startIndex:stopIndex]
  
  # Loop over the other charges of the same PEV and check for overlap
  for (i in 1:nrow(samePEVCharges)) {
    # Find the position of the other charge in the targetSlots
    startIndex <- match(samePEVCharges$Start_Time[i], targetSlots)
    stopIndex <- match(samePEVCharges$Stop_Time[i], targetSlots)
    otherChargeSlots <- targetSlots[startIndex:stopIndex]
    
    # Calculate noise based on charge type
    if (migratedCharge$Charge_Type == "L1") {
      noise <- (length(intersect(migratedChargeSlots, otherChargeSlots)) / 6) * 1.92
    }
    else {
      noise <- (length(intersect(migratedChargeSlots, otherChargeSlots)) / 6) * 6.6
    }
    
    if (noise > 0) return (noise) # Return noise if overlap is detected
  }
  return(0) # Return 0 if no overlap is detected
}

# Checks for overlap with same PEV charges that span midnight and affect the next day
spans.midnight.overlap <- function(df, migratedCharge, targetSlots) {
  midnightIndex <- match("0:00", targetSlots) # Index of midnight (00:00)
  
  # Find the position of the migrated charge in the targetSlots
  startIndex <- match(migratedCharge$Start_Time, targetSlots)
  stopIndex <- match(migratedCharge$Stop_Time, targetSlots)
  
  # If the charge doesn't span midnight, return 0 (no overlap)
  if (startIndex >= midnightIndex || stopIndex < midnightIndex) {
    return(0)
  }
  
  # Adjust the migrated charge's start and stop time to reflect the post-midnight period
  migratedCharge$Start_Time <- targetSlots[midnightIndex]  # Start at midnight
  migratedCharge$Stop_Time <- targetSlots[stopIndex]  # Stop at the original stop time
 
  # Increment date by 1 for the next day (post-midnight)
  nextDay <- as.Date(migratedCharge$Date, format = "%d/%m/%Y") + 1
  nextDay <- sprintf("%d/%d/%d", day(nextDay), month(nextDay), year(nextDay))
  
  # Filter same PEV charges on the next day (post-midnight period)
  samePEVCharges <- filter(df, Charge_Type == migratedCharge$Charge_Type & Date == nextDay & HV_Code == migratedCharge$HV_Code & Time_Zone == migratedCharge$Time_Zone)
  
  # Check for overlap with same PEV charges
  noise <- overlap(samePEVCharges, migratedCharge, targetSlots)
  return (noise)
}

# Create time zone slots for the target zone
# =============================================================================
shoulder1Slots <- create.time.slots("7:00", "13:50")
peakSlots <- create.time.slots("14:00", "19:50")
shoulder2Slots <- create.time.slots("20:00", "21:50")
offPeakSlots <- create.time.slots("22:00", "6:50")

# Prepare Time Zones Data Frame for Load Shifting
# =============================================================================
TZ <- read.csv2("TimeZones.csv", stringsAsFactors = FALSE)
TZ <- TZ %>%
  mutate(
    Index = seq_len(nrow(TZ)),
    Noise = 0
  ) %>%
  select(Index, everything())
TZ_Sort <- TZ[order(dmy(TZ$Date)),]

kwhBefore <- sum(TZ$KWh)

# Filter Data by Time Zone, Charge Type, and DayType, then Split by Date
# =============================================================================
# Filter by Time Zone, Charge Type, and DayType (Weekday) where Splits is FALSE
dailyPeak_L1 <- filter(TZ_Sort, Time_Zone == "Peak" & Charge_Type == "L1" & DayType == "W" & Splits == "FALSE")
dailyPeak_L2 <- filter(TZ_Sort, Time_Zone == "Peak" & Charge_Type == "L2" & DayType == "W" & Splits == "FALSE")

dailyShoulder1_L1 <- filter(TZ_Sort, Time_Zone == "Shoulder 1" & Charge_Type == "L1" & DayType == "W" & Splits == "FALSE")
dailyShoulder1_L2 <- filter(TZ_Sort, Time_Zone == "Shoulder 1" & Charge_Type == "L2" & DayType == "W" & Splits == "FALSE")

dailyShoulder2_L1 <- filter(TZ_Sort, Time_Zone == "Shoulder 2" & Charge_Type == "L1" & DayType == "W" & Splits == "FALSE")
dailyShoulder2_L2 <- filter(TZ_Sort, Time_Zone == "Shoulder 2" & Charge_Type == "L2" & DayType == "W" & Splits == "FALSE")

# == 
# Split the filtered data by Date to create a list of daily subsets
dailyPeak_L1 <- split(dailyPeak_L1, dailyPeak_L1$Date)
dailyPeak_L2 <- split(dailyPeak_L2, dailyPeak_L2$Date)

dailyShoulder1_L1 <- split(dailyShoulder1_L1, dailyShoulder1_L1$Date)
dailyShoulder1_L2 <- split(dailyShoulder1_L2, dailyShoulder1_L2$Date)

dailyShoulder2_L1 <- split(dailyShoulder2_L1, dailyShoulder2_L1$Date)
dailyShoulder2_L2 <- split(dailyShoulder2_L2, dailyShoulder2_L2$Date)

# Initialize Parameters for Load Shifting Model
# =============================================================================
a1 <- 0.1 # (Adjust as needed) percentage of kWh that will migrate from peak.
d1 <- 0.5 # (Do not adjust)    percentage of a1 that will migrate to off peak.
e1 <- 0.2 # (Do not adjust)    percentage of a1 that will migrate to Shoulder 1.
         # Note: the leftover  percentage of a1 will migrate to shoulder 2.
b1 <- 0.1 # (Adjust as needed) percentage of kWh that will migrate from Shoulder 1.
g1 <- 0.5 # (Do not adjust)    percentage of b1 that will migrate to off peak.
         # Note: the leftover  percentage of b1 will migrate to shoulder 2.
c1 <- 0.1 # (Adjust as needed) percentage of kWh that will migrate from Shoulder 2.

# Data Sampling - Load Shifting
# =============================================================================
# dailyPeak_L1 Sampling and Shifting
for (dayData in dailyPeak_L1) {
  dailyIndexes <- dayData$Index  # Extract the indexes for the current day
  TZ <- peak.sample.shifting(TZ, dailyIndexes)  # Sample data and perform load shifting
}

# dailyPeak_L2 Sampling and Shifting
for (dayData in dailyPeak_L2) {
  dailyIndexes <- dayData$Index  # Extract the indexes for the current day
  TZ <- peak.sample.shifting(TZ, dailyIndexes)  # Sample data and perform load shifting
}

# dailyShoulder1_L1 Sampling and Shifting
for (dayData in dailyShoulder1_L1) {
  dailyIndexes <- dayData$Index  # Extract the indexes for the current day
  TZ <- shoulder1.sample.shifting(TZ, dailyIndexes)  # Sample data and perform load shifting
}

# dailyShoulder1_L2 Sampling and Shifting
for (dayData in dailyShoulder1_L2) {
  dailyIndexes <- dayData$Index  # Extract the indexes for the current day
  TZ <- shoulder1.sample.shifting(TZ, dailyIndexes)  # Sample data and perform load shifting
}

# dailyShoulder2_L1 Sampling and Shifting
for (dayData in dailyShoulder2_L1) {
  dailyIndexes <- dayData$Index  # Extract the indexes for the current day
  TZ <- shoulder2.sample.shifting(TZ, dailyIndexes)  # Sample data and perform load shifting
}

# dailyShoulder2_L2 Sampling and Shifting
for (dayData in dailyShoulder2_L2) {
  dailyIndexes <- dayData$Index  # Extract the indexes for the current day
  TZ <- shoulder2.sample.shifting(TZ, dailyIndexes)  # Sample data and perform load shifting
}

# Post-Processing After Load Shifting
# =============================================================================
kwhAfter <- sum(TZ$KWh)
if (kwhBefore == kwhAfter) {
  message("Success: The total kWh before and after load shifting is preserved.")
} else {
  message("Warning: The total kWh has been modified during the load shifting process.")
}

TZ <- TZ %>%
  select(-Index) %>%
  arrange(Charge_Type, HV_Code, dmy(Date), hm(Start_Time))

# Calculate and print 'Noise' percentages (in kWh) for L1 and L2 charge types
sum(TZ$Noise[TZ$Charge_Type %in% "L1"]) / sum(TZ$KWh[TZ$Charge_Type %in% "L1"]) * 100
sum(TZ$Noise[TZ$Charge_Type %in% "L2"]) / sum(TZ$KWh[TZ$Charge_Type %in% "L2"]) * 100

# Export Load Shifting Data Frame to CSV Format (adjust path as needed)
# =============================================================================
write.csv2(TZ, "Loadshifting01.csv", row.names = FALSE) # Path to export file
