#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

library(plyr)
library(ggplot2)

#function to download and unzip the data
downloadFromURLAndUnzip <- function  (fileUrl="http://dir.csv", workdirPath=".", fileName="dataFile")
  # PRE: 3 parameters:
  # 1. url to download
  # 2. the target directory to dump the information
  # 3. the name of the zip file 
  
  # POST: The file will be downloaded and unzip in the targetdirectory (creating the last one if it does not exist)  
{
  fileUnzip<-sub(".zip","",fileName)
  fileAndPath=paste(workdirPath,fileUnzip,sep="/")
  
  print(fileAndPath)
  
  ## Step 0: Checking existence of file
  
  if(!file.exists(fileAndPath)){
    ## Step 1: Checking dir and creating if needed
    if(!file.exists(workdirPath))
    {
      dir.create(workdirPath)
    }
    
    ## Step 2: download the data file
    
    if(!file.exists(fileAndPath))
    {
      download.file(fileUrl, destfile=fileAndPath,method="curl")
      unzip(zipfile=fileAndPath, exdir="./data") 
      dateDownloaded <- date()
      print(paste("INFO: the data file downloaded on: ", dateDownloaded))
    }
  }
}

#calling the function
downloadFromURLAndUnzip("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data","exdata_data_NEI_data")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./data/summarySCC_PM25.rds")

## Keep only the observations from Baltimore and keep the (columns) information needed
NEIBaltimoreSub<-subset(NEI,fips == "24510",select=c(year,type,Emissions)) 


## Sum up the emissions by year and type
NEISumEmissionsByYearType <- ddply(NEIBaltimoreSub, .(year,type), summarise, Emissions=sum(Emissions, na.rm = TRUE))
#NEISumEmissionsByYearType <- ddply(NEISubset, .(year,type), summarise, sumEmissions = sum(Emissions, na.rm = TRUE))

## Using the base plotting system to plot the evolution of the Emissions year by year and type
# Facets in clockwise sense
ggplot(NEISumEmissionsByYearType, aes(year, Emissions))+ facet_wrap(~type)+ ggtitle("Emissions in Baltimore for the different types")+theme(plot.title = element_text(face="bold"))+geom_line()

# All facets in vertical
#ggplot(NEISumEmissionsByYearType, aes(year, Emissions))+ facet_grid(.~type)+ ggtitle("Emissions in Baltimore for the different types")+geom_line()



pngFile <- "plot3.png"
dev.copy(png, file = pngFile)
dev.off()