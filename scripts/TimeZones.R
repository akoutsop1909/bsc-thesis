# Converts L1 and L2 type structures into Time Zones structure

# CONTAINS FUNCTION AT THE END OF THE FILE

# Load packages
# =======================================================
library(ggplot2)
library(lubridate)

# Set Current Working Directory
# =======================================================
setwd("D:/Informatics/BigData")

# Create data frame Time Zones
# =======================================================
TZ_L1 <- convert.to.time.zones("PEV_L1.csv", 1920, "L1")
TZ_L2 <- convert.to.time.zones("PEV_L2.csv", 6600, "L2")

TZ <- rbind (TZ_L1, TZ_L2)
TZ$Time_Zone <- NA
TZ <- TZ[, c(1:6,9,7,8)]

# Check for charge splits
# =======================================================
TZ$Splits <- FALSE

z1 <- c("6:50", "13:50", "19:50", "21:50")
z2 <- c("7:00", "14:00", "20:00", "22:00")

for(i in 2:dim(TZ)[1]) {
  if (TZ$Date[i] == TZ$Date[i-1]) {
    if (TZ$Stop_Time[i-1] %in% z1 & TZ$Start_Time[i] %in% z2) {
      TZ$Splits[i-1] <- TRUE
      TZ$Splits[i] <- TRUE
    }
  }
}

# Percentage of charges that split
sum(TZ$Splits[TZ$Charge_Type%in%"L1"]) / sum(TZ$Charge_Type%in%"L1") * 100
sum(TZ$Splits[TZ$Charge_Type%in%"L2"]) / sum(TZ$Charge_Type%in%"L2") * 100

# Create time zone vectors
# =======================================================
W1 <- seq(dmy_hm("1-1-2010 7:00"), dmy_hm("1-1-2010 13:50"), by = "10 mins")
W1 <- paste(hour(W1), format(W1,"%M"), sep = ":")

W2 <- seq(dmy_hm("1-1-2010 14:00"), dmy_hm("1-1-2010 19:50"), by = "10 mins")
W2 <- format(W2, "%H:%M")

W3 <- seq(dmy_hm("1-1-2010 20:00"), dmy_hm("1-1-2010 21:50"), by = "10 mins")
W3 <- format(W3, "%H:%M")
# ==
H1 <- seq(dmy_hm("1-1-2010 7:00"), dmy_hm("1-1-2010 21:50"), by = "10 mins")
H1 <- paste(hour(H1), format(H1,"%M"), sep = ":")

# Fill Time_Zone column
# =======================================================
for(i in 1:dim(TZ)[1]) {
  
  # DayType is Weekday
  if (TZ$DayType[i] == "W") {
    if (TZ$Start_Time[i] %in% W1) TZ$Time_Zone[i] <- "Shoulder 1"
    else if (TZ$Start_Time[i] %in% W2) TZ$Time_Zone[i] <- "Peak"
    else if (TZ$Start_Time[i] %in% W3) TZ$Time_Zone[i] <- "Shoulder 2"
    else TZ$Time_Zone[i] <- "Off Peak"
  }
  
  # DayType is Weekend / Holiday
  else {
    if (TZ$Start_Time[i] %in% H1) TZ$Time_Zone[i] <- "Shoulder"
    else TZ$Time_Zone[i] <- "Off Peak"
  }
}

# Export the TimeZones data frane to .csv
# =======================================================
write.csv2(TZ, "TimeZones.csv")

# F U N C T I O N S   S E C T I O N
# =======================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = =
convert.to.time.zones <- function(file, watt, type) {
  df <- read.csv2(file)
  df$Time <- dmy_hm(df$Time)
  df$DayType <- format(df$Time, "%a")
  df$DayType <- sub("Mon|Tue|Wed|Thu|Fri", "W", df$DayType)
  df$DayType <- sub("Sat|Sun", "H", df$DayType)
  
  HV_Code <- NA
  Date <- NA
  Charge_Duration <- NA
  Start_Time <- NA
  Stop_Time <- NA
  DayType <- NA
  
  duration <- 0
  k <- 1
  
  tTime <- NA
  z1 <- c("7:00", "14:00", "20:00", "22:00")
  z2 <- c("7:00", "22:00")
  
  lower <- match(dmy_hm("4-1-2010 0:00"), df$Time)
  upper <- match(dmy_hm("8-1-2010 23:50"), df$Time)
  
  for(j in 2:dim(df)[2]) {
    for(i in lower:upper) {
      
      # First Occurrence
      if (df[i,j] == watt & duration == 0) {
        HV_Code[k] <- names(df)[j]
        Date[k] <- paste(day(df$Time[i]), month(df$Time[i]), year(df$Time[i]), sep = "/")
        Start_Time[k] <- paste(hour(df$Time[i]), format(df$Time[i],"%M"), sep = ":")
        DayType[k] <- df$DayType[i]
        
        duration <- 1
        
        # Charge Split
        tTime <- paste(hour(df$Time[i+1]), format(df$Time[i+1],"%M"), sep = ":")
        if ((df$DayType[i+1] == "W" & tTime %in% z1) | (df$DayType[i+1] == "H" & tTime %in% z2)) {
          Charge_Duration[k] <- duration
          Stop_Time[k] <- paste(hour(df$Time[i]), format(df$Time[i],"%M"), sep = ":")
          
          duration <- 0
          k <- k + 1
        }
      }
      
      # Next Occurrences
      else if (df[i,j] == watt & duration != 0) {
        duration <- duration + 1
        
        # Charge Split
        tTime <- paste(hour(df$Time[i+1]), format(df$Time[i+1],"%M"), sep = ":")
        if ((df$DayType[i+1] == "W" & tTime %in% z1) | (df$DayType[i+1] == "H" & tTime %in% z2)) {
          Charge_Duration[k] <- duration
          Stop_Time[k] <- paste(hour(df$Time[i]), format(df$Time[i],"%M"), sep = ":")
          
          duration <- 0
          k <- k + 1
        }
      }
      
      # Last Occurrence
      else if (df[i,j] == 0 & duration != 0) {
        Charge_Duration[k] <- duration
        Stop_Time[k] <- paste(hour(df$Time[i-1]), format(df$Time[i],"%M"), sep = ":")
        
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
