plot4 <- function() {
  
  library(dplyr)
  
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
  
  ##Read in the two data files
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ##subset SCC by choosing rows with both "coal" and "comb" in at lease one column
  coal <- SCC[grepl("comb", SCC$EI.Sector, ignore.case = TRUE)|grepl("comb", SCC$SCC.Level.One, ignore.case = TRUE)|
          grepl("comb", SCC$SCC.Level.Two, ignore.case = TRUE)
          &&grepl("coal", SCC$EI.Sector, ignore.case = TRUE)|grepl("coal", SCC$SCC.Level.Three, ignore.case = TRUE)|
               grepl("coal", SCC$SCC.Level.Four, ignore.case = TRUE), 1]
  
  ##convert factor result to character
  charcoal <- levels(coal)[as.numeric(coal)]
  
  ##subset NEI data based on charcoal vector
  coalNEI <- NEI[NEI$SCC %in% charcoal, ]
  
  ##group results by year
  groupcoal <- group_by(coalNEI, year)
  
  ##sum emissions by year
  summed <- summarise(groupcoal, Emissions = sum(Emissions))
  
  #set graphic device to png
  png("Project2Plot4.png")
  
  ##set up line plot with points and adjust limits
  plot(summed$year, summed$Emissions, xlab = "Year", 
      ylab = "Total Emissions - tons of PM2.5", axes=F, 
      ylim = c(0, 1500000), col = "blue", pch = 15)
  title(main = "Total Coal Combustion Emissions by Year", col.main = "red", font.main=4)
  lines(summed$year, summed$Emissions)
  axis(1, at = c(summed$year), pos = 0)
  axis(2, at = c(0, 500000, 1000000, 1500000), pos = 1998.9)

  ##close connection
  dev.off()
  
}
  