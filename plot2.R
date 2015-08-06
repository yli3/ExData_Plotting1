## plot2.R
#
# This file contains functions used to produce the plot specified for
# Plot 2 of Course Project 1 in Coursera EXDATA-031.
# Usage: source, and call plot2()
# Output: plot2.png

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

# plot2
#
# Line plot of Global Active Power (kW) vs datetime using data from data.txt
# over the course of two days (February 1-2, 2007), and 
# saves output as PNG.
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
  datetime <- as.POSIXct(paste(dt$Date, dt$Time),
    format = "%d/%m/%Y %H:%M:%S")
                           
  # Produce plot.
  plot(datetime, as.numeric(dt$Global_active_power),
    type = "l",
    xlab = "",
    ylab = "Global Active Power (kilowatts)"
  )
  
  # Close graphics device.
  invisible(dev.off())
}