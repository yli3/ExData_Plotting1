## plot3.R
#
# This file contains functions used to produce the plot specified for
# Plot 3 of Course Project 1 in Coursera EXDATA-031.
# Usage: source, and call plot3()
# Output: plot3.png

# readData
#
# Reads data.txt and returns data from February 1 and 2, 2007
# as a data.table for use by other functions.
#
# Args:
#   None.
# Return:
#   data.table

readData <- function() {
  # Load libraries required.
  library("data.table")
  
  # Read data.
  dt <- fread("data.txt",
    colClasses="character",
    na.strings="?"
  )
  
  # Subset for dates of interest.
  validDates <- c("1/2/2007", "2/2/2007")
  
  return(dt[Date %in% validDates])
}

# plot3
#
# Line plot of energy sub metering vs datetime using data from data.txt over 
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
  datetime <- as.POSIXct(paste(dt$Date, dt$Time),
                           format = "%d/%m/%Y %H:%M:%S")
  
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