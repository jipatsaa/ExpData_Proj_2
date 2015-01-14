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