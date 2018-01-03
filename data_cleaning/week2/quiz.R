library("RMySQL")
library("jsonlite")
library(rhdf5)
library("XML") #can use this to parse HTML as HTML is XML
library("httr")
library("sqldf")
library("readr")

question1 <- function(){
  twitterURL <- "https://api.twitter.com/1.1/statuses/home_timeline.json"
  # docs for this API https://developer.twitter.com/en/docs/api-reference-index
  myapp <- oauth_app(
    "github", 
    key="34ff232de2138c247fb7", 
    secret="fbaedba912d18112c755af65e606ef0548ff7d10"
  )
  github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
  gtoken <- config(token = github_token)
  homeTL <- GET("https://api.github.com/users/jtleek/repos", gtoken)
  jsonRaw <- content(homeTL)
  jsonData <- fromJSON(toJSON(jsonRaw))
  return(jsonData)
}

question2and3 <- function(){
  rawCSV <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv","./data.csv");
  acs <- data.table::fread("./data.csv");
  a <- sqldf("select pwgtp1 from acs where AGEP < 50")
  print(a)
  b <- sqldf("select AGEP where unique from acs")
  print(b)
}

question4 <- function(){
  scrapeUrl <- "http://biostat.jhsph.edu/~jleek/contact.html"
  con = url(scrapeUrl)
  htmlCode = readLines(con)
  firstAnswer <- nchar(htmlCode[10])
  cat("10th line: ",firstAnswer,"\n")
  secondAnswer <- nchar(htmlCode[20])
  cat("20th line: ",secondAnswer,"\n")
  thirdAnswer <- nchar(htmlCode[30])
  cat("30th line: ",thirdAnswer,"\n")
  fourthAnswer <- nchar(htmlCode[100])
  cat("100th line: ",fourthAnswer,"\n")
  
  #html2 <- GET(scrapeUrl)
  #content2 <- content(html2, as="text")
  #parsedHtml <- htmlParse(content2, asText=TRUE)
  #return(parsedHtml)
  close(con)
}

question5 <- function(){
  forFile <- read.fwf(
    file=url("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"),
    skip=4,
    widths=c(12, 7, 4, 9, 4, 9, 4, 9, 4)
  )
  
  answer <- sum(forFile[[4]])
  
  print(answer)
}