## plot4.R
#
# This file contains functions used to produce the plots specified for
# Plot 4 of Course Project 1 in Coursera EXDATA-031.
# Usage: source, and call plot4()
# Output: plot4.png

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

# plot4
#
# Plots a series of 4 line plots in 2x2 format using data from data.txt over the
# course of two days (February 1-2, 2007), and saves the output as PNG.
#
# As Plot[Row, Column]:
#   Plot[1, 1]: Global Active Power ~ datetime
#   Plot[1, 2]: Voltage ~ datetime
#   Plot[2, 1]: Energy sub metering ~ datetime (Sub_metering_1, _2, and _3)
#   Plot[2, 2]: Global Reactive Power ~ datetime
#
# Args:
#   None.
# Return:
#   dev.off(): number and name of new active graphics device
# Output:
#   plot4.png: a 480x480 PNG file.

plot4 <- function() {
  # Get data.
  dt <- readData()

  # Open graphics device.
  png("plot4.png", width = 480, height = 480)
  
  # Get vector of POSIXct datetimes from data.
  datetime <- as.POSIXct(paste(dt$Date, dt$Time), format = "%d/%m/%Y %H:%M:%S")
  
  # 2x2 base plots, to be defined by column.
  par(mfrow = c(2, 2))
  
  # Plot[1,1]: Global Active Power ~ datetime.
  plot(datetime, as.numeric(dt$Global_active_power),
    type = "l",
    xlab = "",
    ylab = "Global Active Power"
  )
  
  # Plot[1,2]: Voltage ~ datetime.
  plot(datetime, as.numeric(dt$Voltage),
    type = "l",
    ylab = "Voltage"
  )
  
  # Plot[2,1]: {Sub_metering_1, _2, and _3} ~ datetime.
  plot(datetime, as.numeric(dt$Sub_metering_1),
    type = "l",
    xlab = "",
    ylab = "Energy sub metering"
  ) 
  
  points(datetime, as.numeric(dt$Sub_metering_2),
    type = "l",
    col = "red"
  ) 
  
  points(datetime, as.numeric(dt$Sub_metering_3),
    type = "l",
    col = "blue"
  )
  
  legend("topright", 
    c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
    col = c("black", "red", "blue"),
    lty = c(1, 1, 1),
    bty = "n"
  )
  
  # Plot[2,2]: Global_reactive_power ~ datetime.
  plot(datetime, as.numeric(dt$Global_reactive_power),
    type = "l",
    ylab = "Global_reactive_power"
  )
 
  # Close graphics device.
  invisible(dev.off())
}