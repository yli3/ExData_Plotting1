## plot4.R
#
# This file contains functions used to produce the plots specified for
# Plot 4 of Course Project 1 in Coursera EXDATA-031.
#
# It uses a household_power_consumption.txt comma-separated data file that is
# acquired from online if it does not currently exist in the directory.
#
# Usage: source, and call plot4()
# Output: plot4.png

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

# plot4
#
# Plots a series of 4 line plots in 2x2 format using data from file over the
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