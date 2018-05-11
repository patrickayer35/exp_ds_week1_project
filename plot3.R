form_plot3 <- function()
{
	library(dplyr)
	library(lubridate)
	hd_raw <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = "character")
	hd_tidy <- tbl_df(hd_raw)
	hd <- filter(hd_tidy, Date == "1/2/2007" | Date == "2/2/2007")
	hd[, 3:9] <- as.double(unlist(hd[, 3:9]))
	hd <- mutate(hd, Count = 1:nrow(hd))
	
	x <- hd$Count
	y1 <- hd$Sub_metering_1
	y2 <- hd$Sub_metering_2
	y3 <- hd$Sub_metering_3
	
	png("plot3.png")
	plot(x, y1, type = "l", col = "black", ylab = "Energy sub metering", xlab = "", xaxt = "n")
	lines(x, y2, col = "red")
	lines(x, y3, col = "blue")
	axis(side = 1, at = c(1, 1441, 2881), labels = c("Thu", "Fri", "Sat"))
	legend(x = "topright",
		   y = NULL,
		   legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		   #bty = "n",
		   lty = 1,
		   col = c("black", "red", "blue"))
	dev.off()
}