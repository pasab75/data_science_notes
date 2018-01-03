library("plyr")
#library("Hmisc")
library("dplyr")
#library("jpeg")
library("data.table")
library("quantmod")

question1 <- function(){
  fileName <- "./data/microSurvey.csv"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
    download.file(fileUrl,destfile = fileName)
  }
  microSurvey <- read.csv(fileName)
  splitNames <- strsplit(names(microSurvey),"wgtp")
  print(splitNames[120:123])
}

question2 <- function(){
  fileName <- "./data/GDP.csv"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(fileUrl,destfile = fileName)
  }
  gdp <- read.csv(fileName, skip=4, nrows=190);
  gdp <- rename(
    gdp, 
    CountryCode = X, 
    Rank = X.1, 
    Country = X.3,
    gdp = X.4
  )
  print(head(gdp))
  gdpList <- as.numeric(gsub(",","",gdp$gdp))
  print(mean(gdpList, na.rm=TRUE))
}

question3 <- function(){
  fileName <- "./data/GDP.csv"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName)){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
    download.file(fileUrl,destfile = fileName)
  }
  gdp <- read.csv(fileName, skip=4, nrows=190);
  gdp <- rename(
    gdp, 
    CountryCode = X, 
    Rank = X.1, 
    Country = X.3,
    gdp = X.4
  )
  unitedList <- grepl("^United", gdp$Country)
  print(summary(unitedList))
}

question4 <- function() {
  gdp <- data.table::fread('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
                               , skip=5
                               , nrows=190
                               , select = c(1, 2, 4, 5)
                               , col.names=c("CountryCode", "Rank", "Country", "GDP")
  )
  
  dt <- data.table::fread('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv')
  mergedDT <- merge(gdp, dt, by = 'CountryCode')
  print(mergedDT[grepl(pattern = "Fiscal year end: June 30;", mergedDT[, `Special Notes`]), .N])
}

question5 <- function(){
  a <- getSymbols("AMZN",auto.assign=FALSE)
  sampleTimes <- index(a) 
  dt <- data.table(timeCol = sampleTimes)
  
  # how many samples occured in 2012
  print(dt[(timeCol >= "2012-01-01") & (timeCol) < "2013-01-01", .N ])
  
  # How many samples did we see on Mondays in 2012?
  print(dt[
    (
      (timeCol >= "2012-01-01") & 
        (timeCol < "2013-01-01")
    ) & 
    (
      weekdays(timeCol) == "Monday"
    ), 
    .N 
  ])
}