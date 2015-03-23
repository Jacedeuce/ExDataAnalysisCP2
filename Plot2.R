plot2 <- function() {
  
library(dplyr)
library(calibrate)

if(!file.exists("summarySCC_PM25.rds")){
  ##Data source
  dataURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  ##name downloaded file
  zipName <- "FNEIdata.zip"
  ##download the file from the source
  download.file(dataURL, zipName, method = "curl")
  
  ##Unzip the file into the set working directory
  unzip(zipName)
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI <- NEI[NEI$fips == "24510", ]
NEI <- NEI[, c(4:6)]

grouped <- group_by(NEI, year)
summed <- summarise(grouped, Emissions = sum(Emissions))

png("Project2Plot2.png")

plot(summed$year, summed$Emissions, xlab = "Year", ylab = "Baltimore City Emissions - tons of PM2.5", axes=F, col = "blue", pch = 15,
     ylim = c(0, max(summed$Emissions)))
title(main = "Total Baltimore City Emissions by Year \nFIPS: 24510", col.main = "red", font.main=4)
lines(summed$year, summed$Emissions)
axis(1, at = c(summed$year), pos = 0)
axis(2, at = c(0, 3300/4, 3300/2, 3300/4*3, 3300), pos = 1998.9)

dev.off()

}