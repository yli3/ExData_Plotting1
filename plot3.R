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
  
  
  # Read data.
  dt <- fread("household_power_consumption.txt",
    colClasses="character",
    na.strings="?"
  )
  
  # Subset for dates of interest.
  validDates <- c("1/2/2007", "2/2/2007")
  
  return(dt[Date %in% validDates])
}

# plot3
#
# Line plot of energy sub metering vs datetime using data from file over 
# the course of two days (February 1-2, 2007), and saves output as PNG.
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