form_plot4 <- function()
{
	library(dplyr)
	library(lubridate)
	hd_raw <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = "character")
	hd_tidy <- tbl_df(hd_raw)
	hd <- filter(hd_tidy, Date == "1/2/2007" | Date == "2/2/2007")
	hd[, 3:9] <- as.double(unlist(hd[, 3:9]))
	hd <- mutate(hd, Count = 1:nrow(hd))
	
	x <- hd$Count
	y1 <- hd$Global_active_power
	y2 <- hd$Voltage
	y3_1 <- hd$Sub_metering_1
	y3_2 <- hd$Sub_metering_2
	y3_3 <- hd$Sub_metering_3
	y4 <- hd$Global_reactive_power
	
	png("plot4.png")
	
	par(mfrow = c(2, 2))
	
	#plot1
	plot(x, y1, type = "l", ylab = "Global Active Power", xlab = "", xaxt = "n")
	axis(side = 1, at = c(1, 1441, 2881), labels = c("Thu", "Fri", "Sat"))
	
	#plot2
	plot(x, y2, type = "l", ylab = "Voltage", xlab = "datetime", xaxt = "n")
	axis(side = 1, at = c(1, 1441, 2881), labels = c("Thu", "Fri", "Sat"))
	
	#plot3
	plot(x, y3_1, type = "l", col = "black", ylab = "Energy sub metering", xlab = "", xaxt = "n")
	lines(x, y3_2, col = "red")
	lines(x, y3_3, col = "blue")
	legend(x = "topright",
		   y = NULL,
		   legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		   bty = "n",
		   lty = 1,
		   col = c("black", "red", "blue"))
	axis(side = 1, at = c(1, 1441, 2881), labels = c("Thu", "Fri", "Sat"))
	
	#plot4
	plot(x, y4, type = "l", ylab = "Global_reactive_power", xlab = "datetime", xaxt = "n")
	axis(side = 1, at = c(1, 1441, 2881), labels = c("Thu", "Fri", "Sat"))
	
	dev.off()
}













