## plot3.R
#
# This file contains functions used to produce the plot specified for
# Plot 3 of Course Project 1 in Coursera EXDATA-031.
#
# It uses a household_power_consumption.txt comma-separated data file that is
# acquired from online if it does not currently exist in the directory.
#
# Usage: source, and call plot3()
# Output: plot3.png

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

# plot3
#
# Line plot of energy sub metering vs datetime using data obtained by
# readData(), and saves output as PNG.
#
# Three variables plotted: Sub_metering_1, Sub_metering_2, and Sub_metering_3.
#
# Args:
#   None.
# Return:
#   dev.off(): number and name of new active graphics device
# Output:
#   plot3.png: a 480x480 PNG file.


plot3 <- function() {
  # Get data.
  dt <- readData()
  
  # Open graphics device.
  png("plot3.png", width = 480, height = 480)
  
  # Get vector of POSIXct datetimes from data.
  datetime <- as.POSIXct(paste(dt$Date, dt$Time), format = "%d/%m/%Y %H:%M:%S")
  
  # Plot Sub_metering_1.
  plot(datetime, as.numeric(dt$Sub_metering_1),
    type = "l",
    xlab = "",
    ylab = "Energy sub metering"
  )
  
  # Add Sub_metering_2.
  points(datetime, as.numeric(dt$Sub_metering_2),
    type = "l",
    col = "red"
  )
  
  # Add Sub_metering_3.
  points(datetime, as.numeric(dt$Sub_metering_3),
    type = "l",
    col = "blue"
  )

  # Add legend.
  legend("topright", 
    c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
    col = c("black", "red", "blue"),
    lty = c(1, 1, 1)
  )

  # Close graphics device.
  invisible(dev.off())
}