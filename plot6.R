## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City? 
library(plyr)
library(ggplot2)

#function to change the title of the facets later on
mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="fips") { 
    value[value=="24510"] <- "Baltimore"
    value[value=="06037"]   <- "Los ANgeles"
  }
  return(value)
}


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
NEISubset<-subset(NEI,select=c(fips,year,SCC,Emissions)) 
SCCSubset<-subset(SCC,select=c(SCC,EI.Sector))

## Merge both data, adding to NEI information the source text matching the SCC
## (originally in NEI appears the source code and in SCC the source code and the equivalent text)
NEI_SCC<-merge(SCCSubset,NEISubset,by="SCC")

## Keep only the observations from Vehicle related sources in Baltimore(MA) and in Los Angeles
NEI_SCC_Vehicles<-subset(NEI_SCC,grepl("Vehicle",NEI_SCC$EI.Sector, ignore.case = TRUE)&(fips == "24510"|fips == "06037"))

## Sum up the emissions by year
NEIVehiclesSumEmissionsByYear <- ddply(NEI_SCC_Vehicles, .(year,fips), summarise, sumEmissions = sum(Emissions, na.rm = TRUE))

## Using the base plotting system to plot the evolution of the Emissions year by year
## The plot shows that the Emissions decend in Baltimore while in Los Angeles that only hapenned since 2006
ggplot(NEIVehiclesSumEmissionsByYear, aes(year, sumEmissions))+ ggtitle("Emissions from vehicle sources in Los Angeles vs. Baltimore")+theme(plot.title = element_text(face="bold"))+facet_grid(.~fips, labeller=mf_labeller)+geom_point()+geom_line()

pngFile <- "plot6.png"
dev.copy(png, file = pngFile)
dev.off()