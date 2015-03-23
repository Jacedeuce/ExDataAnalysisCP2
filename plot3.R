plot3 <- function() {
  
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

##Subset for Baltimore City
NEI <- NEI[NEI$fips == "24510", ]

##Subset for columns "Emissions", "type" and "year"
NEI <- dplyr::select(NEI, Emissions:year)

##group by type and year and calculate the sum of each key value pair
groupNEI <- NEI %>% group_by(type, year) %>% summarise_each(funs(sum))


##set output device to PNG
png("Project2Plot3.png")

## create plot with different colored lines for each type of emissions collection
p <- ggplot(groupNEI, aes(x=year, y=Emissions, colour=type)) 
      print(p +
  geom_line() + geom_point() + ggtitle("Total PM2.5 emissions by Source Type and Year") + 
  theme(legend.position="top", plot.title = element_text(lineheight=.8, face="bold")) +xlab("Year") + 
  ylab("Total PM2.5 Emissions") + scale_colour_discrete(name="Source Type") + 
  scale_x_continuous(breaks=seq(1999, 2008, 3)))


##turn off connection to output device
dev.off()

}