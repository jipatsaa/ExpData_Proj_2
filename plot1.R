library(plyr)

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

## Keep only the information needed
NEISubset<-subset(NEI,select=c(year,Emissions)) 

## Sum up the emissions by year
NEISumEmissionsByYear <- ddply(NEISubset, .(year), summarise, sumEmissions = sum(Emissions, na.rm = TRUE))

## Using the base plotting system to plot the evolution of the Emissions year by year
## The plot shows that the Emissions decend
par(mar=c(4,4,3,1))
plot(NEISumEmissionsByYear$year,NEISumEmissionsByYear$sumEmissions,type="l",xlab="Year",ylab="PM25-PRI Emissions",main="Emissions by Year",xlim=c(1998, 2008))
points(NEISumEmissionsByYear$year,NEISumEmissionsByYear$sumEmissions,pch=2)#adding triangles showing the "real data points"

pngFile <- "plot1.png"
dev.copy(png, file = pngFile)
dev.off()