form_plot1 <- function()
{
	library(dplyr)
	library(lubridate)
	hd <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = "character")
	hd_tidy <- tbl_df(hd)
	hd_sel <- filter(hd_tidy, Date == "1/2/2007" | Date == "2/2/2007")
	
	hd_sel$Date <- dmy(hd_sel$Date)
	hd_sel$Time <- strptime(hd_sel$Time, format = "%H:%M:%S")
	hd_sel[, 3:9] <- as.double(unlist(hd_sel[, 3:9]))
	
	png("plot1.png")
	plot1 <- hist(hd_sel$Global_active_power,
				  col = "red",
				  ylim = c(0, 1200),
				  main = "Global Active Power",
				  xlab = "Global Active Power (kilowatts)")
	dev.off()	
}