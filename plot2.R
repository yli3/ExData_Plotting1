## plot2.R
#
# This file contains functions used to produce the plot specified for
# Plot 2 of Course Project 1 in Coursera EXDATA-031.
#
# It uses a household_power_consumption.txt comma-separated data file that is
# acquired from online if it does not currently exist in the directory.
#
# Usage: source, and call plot2()
# Output: plot2.png

# readData
#
# Acquires data file if needed, and reads data as a data.table, returning
# rows from February 1 and 2, 2007.
#
# Notes: 
#   Although fread is very fast on the entire data file, this function now
#   skips rows while reading in the data frame to demonstrate it is possible.
#   To see its original state, please refer to previous commits, for example
#   SHA-1: 2064f1679525e2f32b45d184da109f49182ba16f.
# Args:
#   None.
# Return:
#   data.table

readData <- function() {
  # Load libraries required.
  library("data.table")
  
  # download file if not present
  if(!file.exists('./household_power_consumption.txt')) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(fileUrl, "./household_power_consumption.zip", mode = "wb")
    unzip("./household_power_consumption.zip")
  }
  
  # Read data, skipping to desired rows.
  dt <- fread("household_power_consumption.txt",
    colClasses = "character",
    na.strings = "?",
    header = TRUE,
    skip = (6 * 60) + (36) + (46 * 60 * 24),
    nrow = (2 * 60 * 24)
  )
  
  setnames(dt, c(
    "Date",
    "Time",
    "Global_active_power",
    "Global_reactive_power",
    "Voltage",
    "Global_intensity",
    "Sub_metering_1",
    "Sub_metering_2",
    "Sub_metering_3")
  )
  
  return(dt)
}

# plot2
#
# Line plot of Global Active Power (kW) vs datetime using data.table obtained
# by readData(), and saves output as PNG.
#
# Args:
#   None.
# Return:
#   dev.off(): number and name of new active graphics device
# Output:
#   plot2.png: a 480x480 PNG file.

plot2 <- function() {
  # Get data.
  dt <- readData()
  
  # Open graphics device.
  png("plot2.png", width = 480, height = 480)
  
  # Get vector of POSIXct datetimes from data.
  datetime <- as.POSIXct(paste(dt$Date, dt$Time), format = "%d/%m/%Y %H:%M:%S")
  
  # Produce plot.
  plot(datetime, as.numeric(dt$Global_active_power),
    type = "l",
    xlab = "",
    ylab = "Global Active Power (kilowatts)"
  )
  
  # Close graphics device.
  invisible(dev.off())
}