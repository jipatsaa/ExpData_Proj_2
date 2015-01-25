## Across the United States, how have emissions from coal combustion-related sources changed 
## from 1999–2008?
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
SCC<- readRDS("./data/Source_Classification_Code.rds")

## Keep only the information needed
NEISubset<-subset(NEI,select=c(year,SCC,Emissions)) 
SCCSubset<-subset(SCC,select=c(SCC,EI.Sector))

## Merge both data, adding to NEI information the source text matching the SCC
## (originally in NEI appears the source code and in SCC the source code and the equivalent text)
NEI_SCC<-merge(SCCSubset,NEISubset,by="SCC")

## Keep only the observations from Coal combustion related sources
NEI_SCC_Coal<-subset(NEI_SCC,grepl("Coal",NEI_SCC$EI.Sector), ignore.case = TRUE))

## Sum up the emissions by year
NEICoalSumEmissionsByYear <- ddply(NEI_SCC_Coal, .(year), summarise, sumEmissions = sum(Emissions, na.rm = TRUE))

## Using the base plotting system to plot the evolution of the Emissions year by year
## The plot shows that the Emissions decend
ggplot(NEICoalSumEmissionsByYear, aes(year, sumEmissions))+ ggtitle("Emissions from coal combustion-related sources")+theme(plot.title = element_text(face="bold"))+geom_point()+geom_line()

pngFile <- "plot4.png"
dev.copy(png, file = pngFile)
dev.off()