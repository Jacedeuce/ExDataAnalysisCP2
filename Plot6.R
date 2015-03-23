plot6 <- function() {
  
  library(dplyr)
  library(ggplot2)
  
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
  BaltNEI <- NEI[NEI$fips == "24510"|NEI$fips == "06037", ]
  
  ##use GREP to create a vector with vehicles from SCC data
  vehicles <- SCC[grepl("vehicles", SCC$EI.Sector, ignore.case = TRUE)|grepl("vehicles", SCC$SCC.Level.One, ignore.case = TRUE)|
                    grepl("vehicles", SCC$SCC.Level.Two, ignore.case = TRUE)|grepl("vehicles", SCC$EI.Sector, ignore.case = TRUE)|
                    grepl("vehicles", SCC$SCC.Level.Three, ignore.case = TRUE)|
                    grepl("vehicles", SCC$SCC.Level.Four, ignore.case = TRUE), 1]
  
  ##convert factor result to character
  charvehicles <- levels(vehicles)[as.numeric(vehicles)]
  
  ##subset NEI data based on vehicles vector
  BaltVehNEI <- BaltNEI[BaltNEI$SCC %in% charvehicles, ]
  ##AngelVehNEI <- AngelNEI[AngelNEI$SCC %in% charvehicles, ]
  
  ##remove unnecessary columns for plot
  BaltVehNEI <- BaltVehNEI[, c(1, 4, 6)]
  ##AngelVehNEI <- AngelVehNEI[, c(4:6)]
  
  ##group by year
  Bgrouped <- group_by(BaltVehNEI, fips, year)
  ##Agrouped <- group_by(AngelVehNEI, year)
  
  ##sum Emissions by year
  Bsummed <- summarise(Bgrouped, Emissions = sum(Emissions))
  ##Asummed <- summarise(Agrouped, Emissions = sum(Emissions))
  
  ##Set graphic device to .png
  png("Project2Plot6.png")
  
  ##create a point and line plot and reset title and axis labels and limits
  p <- ggplot(Bsummed, aes(x=year, y=Emissions, colour=fips)) 
  print(p +
          geom_line() + geom_point() + 
          ggtitle("Vehicle PM2.5 emissions by Year for LA and Baltimore\n
                  Baltimore FIPS = 24510 : LA FIPS = 06037") + 
          theme(legend.position="top", plot.title = element_text(lineheight=.8, face="bold")) +xlab("Year") + 
          ylab("Total Vehicle PM2.5 Emissions") +
          scale_colour_discrete(name="City", labels=c("LA", "Baltimore")) + 
          scale_x_continuous(breaks=seq(1999, 2008, 3)))
  
  ##close connection
  dev.off()
  
}