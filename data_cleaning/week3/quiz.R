library("plyr")
library("Hmisc")
library("dplyr")
library("jpeg")
library("data.table")

question1 <- function(){
  fileName1 <- "./data/survey2006.csv"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName1)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(fileUrl,destfile = fileName1)
  }
  survey2006 <- read.csv(fileName1);
  print(names(survey2006))
  print(which(survey2006$ACR == 3 & survey2006$AGS == 6))
}

question2 <- function(){
  fileName1 <- "./data/jeff.jpg"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName1)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
    download.file(fileUrl, mode="wb", destfile = fileName1)
  }
  jeffPicture <- jpeg::readJPEG(fileName1,native=TRUE)
  quantiles <- quantile(jeffPicture, c(.30, .80))
  print(quantiles)
}

question3 <- function(){
  fileName1 <- "./data/gdp.csv"
  fileName2 <- "./data/fedStats.csv"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName1)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(fileUrl,destfile = fileName1)
  }
  if(!file.exists(fileName2)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
    download.file(fileUrl,destfile = fileName2)
  }
  
  gdp <- read.csv(fileName1, skip=4, nrows=190);
  
  fedStats <- read.csv(fileName2);
  gdp <- rename(
    gdp, 
    CountryCode = X, 
    Rank = X.1, 
    Economy = X.3,
    Total = X.4
  )
  gdp <- select(gdp, CountryCode:Total)
  gdp <- select(gdp, -(X.2))
  print(names(gdp))
  print(nrow(gdp))
  print(ncol(gdp))
  #print(names(fedStats))
  gdpMerged <- merge(gdp, fedStats,by="CountryCode")
  gdpMerged <- arrange(gdpMerged, desc(Rank))
  print(nrow(gdpMerged))
  print(ret[13,])
}

