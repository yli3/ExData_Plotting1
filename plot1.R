## plot1.R
#
# This file contains functions used to produce the plot specified for
# Plot 1 of Course Project 1 in Coursera EXDATA-031.
# Usage: source, and call plot1()
# Output: plot1.png

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

# plot1
#
# Histogram plot of Global Active Power using data from data.txt
# over the course of two days (February 1-2, 2007), and 
# saves output as PNG.
#
# Args:
#   None.
# Return:
#   dev.off(): number and name of new active graphics device
# Output:
#   plot1.png: a 480x480 PNG.

plot1 <- function() {
  # Get data.
  dt <- readData()
  
  # Open graphics device.
  png("plot1.png", width = 480, height = 480)
  
  # Produce plot.
  hist(as.numeric(dt$Global_active_power),
    main = "Global Active Power",
    xlab = "Global Active Power (kilowatts)",
    ylab = "Frequency",
    col = "red"
  )
  
  # Close graphics device.
  invisible(dev.off())
}