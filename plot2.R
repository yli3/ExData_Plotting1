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

# plot2
#
# Line plot of Global Active Power (kW) vs datetime using data from file
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