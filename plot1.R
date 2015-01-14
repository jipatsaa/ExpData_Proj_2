source("downloadFromURLAndUnzip.R")
downloadFromURLAndUnzip("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip","data","exdata_data_NEI_data")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
SCCSubset<-subset(SCC,select=c(SCC,Short.Name))
data<-merge(SCCSubset,NEI,by="SCC")
dataSumEmissionsByYear <- ddply(data, .(year), summarise, sumEmissions = sum(Emissions, na.rm = TRUE))
plot(dataSumEmissionsByYear$year,dataSumEmissionsByYear$sumEmissions,type="l",xlab="Year",ylab="PM25-PRI Emissions",main="Emissions by Year")
pngFile <- "plot1.png"
dev.copy(png, file = pngFile,  bg = "white")
dev.off()