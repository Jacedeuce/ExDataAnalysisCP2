plot5 <- function() {
  
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
  
  ##Read in the .rds data
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ##Subset for Baltimore City
  NEI <- NEI[NEI$fips == "24510", ]
  
  ##use GREP to create a vector with vehicles from SCC data
  vehicles <- SCC[grepl("vehicles", SCC$EI.Sector, ignore.case = TRUE)|grepl("vehicles", SCC$SCC.Level.One, ignore.case = TRUE)|
                grepl("vehicles", SCC$SCC.Level.Two, ignore.case = TRUE)|grepl("vehicles", SCC$EI.Sector, ignore.case = TRUE)|
                grepl("vehicles", SCC$SCC.Level.Three, ignore.case = TRUE)|
                grepl("vehicles", SCC$SCC.Level.Four, ignore.case = TRUE), 1]
  
  ##convert factor result to character
  charvehicles <- levels(vehicles)[as.numeric(vehicles)]
  
  ##subset NEI data based on vehicles vector
  vehiclesNEI <- NEI[NEI$SCC %in% charvehicles, ]
  
  ##remove unnecessary columns for plot
  vehiclesNEI <- vehiclesNEI[, c(4:6)]
  
  ##group by year
  grouped <- group_by(vehiclesNEI, year)
  
  ##sum Emissions by year
  summed <- summarise(grouped, Emissions = sum(Emissions))
  
  ##Set graphic device to .png
  png("Project2Plot5.png")
  
  ##create a point and line plot and reset title and axis labels and limits
  plot(summed$year, summed$Emissions, xlab = "Year", ylab = "Baltimore City Emissions - tons of PM2.5", axes=F, col = "blue", pch = 15,
       ylim = c(0, max(summed$Emissions)))
    title(main = "Total Baltimore City Motor Vehicle Emissions by Year \nFIPS: 24510", col.main = "red", font.main=4)
    lines(summed$year, summed$Emissions)
    axis(1, at = c(summed$year), pos = 0)
    axis(2, at = c(0, 350/4, 350/2, 350/4*3, 350), pos = 1998.9)
  
  ##close connection
  dev.off()
  
}