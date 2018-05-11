form_plot2 <- function()
{
	library(dplyr)
	library(lubridate)
	hd_raw <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = "character")
	hd_tidy <- tbl_df(hd_raw)
	hd <- filter(hd_tidy, Date == "1/2/2007" | Date == "2/2/2007")
	hd[, 3:9] <- as.double(unlist(hd[, 3:9]))
	hd <- mutate(hd, Count = 1:nrow(hd))
	
	x <- hd$Count
	y <- hd$Global_active_power
	
	png("plot2.png")
	plot2 <- plot(x, y, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "", xaxt = "n")
	axis(side = 1, at = c(1, 1441, 2881), labels = c("Thu", "Fri", "Sat"))
	dev.off()
}