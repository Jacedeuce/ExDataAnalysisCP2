plot1 <- function() {
  
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

dplyr::select(NEI, Emissions, year)

grouped <- group_by(NEI, year)
summed <- summarise(grouped, Emissions = sum(Emissions))

png("Project2Plot1.png")

plot(summed$year, summed$Emissions, xlab = "Year", ylab = "Total Emissions - tons of PM2.5", axes=F, ylim = c(0, 7500000), col = "blue", pch = 15)
title(main = "Total Emissions by Year", col.main = "red", font.main=4)
lines(summed$year, summed$Emissions)
axis(1, at = c(summed$year), pos = 0)
axis(2, at = c(0, 2500000, 5000000, 7500000), pos = 1998.9)

dev.off()

}