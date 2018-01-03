library(data.table)
library(xlsx)
library(XML)
# 
problem1 <- function(){
  #rawCSV <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv","./data.csv");
  datacsv <- data.table::fread("./data.csv");
  completeCSV <- datacsv[complete.cases(datacsv$VAL)];
  #v <- completeCSV[["VAL"]];
  enrichedCSV <- completeCSV[, MILLIONARE:={VAL>=24} ];
  millionares <- sum(enrichedCSV$MILLIONARE == TRUE);
}

problem3 <- function(){
  #rawCSV <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx","./data.xlsx", mode="wb");
  dat <- read.xlsx("./data.xlsx", sheetIndex=1, header=TRUE, colIndex=7:15, rowIndex=18:23);
  sum(dat$Zip*dat$Ext,na.rm=T)
}

problem4 <- function(){
  #rawdata <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml", "./data.xml");
  doc <- xmlTreeParse('./data.xml',useInternal=TRUE); 
  rootNode <- xmlRoot(doc);
  zipcodes <- xpathSApply(rootNode, "//zipcode", xmlValue);
  sum(zipcodes==21231)
}

problem5 <- function(){
  rawCSV <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv","./data2.csv");
  DT <- data.table::fread("./data2.csv");
  DT <- fread("./data2.csv")
  file.info("./data2.csv")$size
  system.time(DT[,mean(pwgtp15),by=SEX])
  system.time(mean(DT[DT$SEX==1,]$pwgtp15))+system.time(mean(DT[DT$SEX==2,]$pwgtp15))
  system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
  system.time(mean(DT$pwgtp15,by=DT$SEX))
  system.time(tapply(DT$pwgtp15,DT$SEX,mean))
  system.time(rowMeans(DT)[DT$SEX==1])+system.time(rowMeans(DT)[DT$SEX==2])
}