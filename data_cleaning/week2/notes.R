library("RMySQL")
library("jsonlite")
library(rhdf5)
library("XML") #can use this to parse HTML as HTML is XML
library("httr")

mySqlNotes <- function(){
  hg19 <- dbConnect(
    MySQL(), 
    user="genome", 
    host="genome-mysql.cse.ucsc.edu", db="hg19"
  );
  
  #ucscDb <- dbConnect(
  #  MySQL(), 
  #  user="genome", 
  #  host="genome-mysql.cse.ucsc.edu"
  #);
  
  #result <- dbGetQuery(
  #  ucscDb, 
  #  "show databases;"
  #)
  
  allTables <- dbListTables(hg19);
  hg19Fields <- dbListFields(hg19, "affyU133Plus2")
  hg19AffyCount <- dbGetQuery(hg19, "select count(*) from affyU133Plus2")
  query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
  affyMis <- fetch(query);
  misMatches <- quantile(affyMis$misMatches); #stats on DB
  affyMisSmall <- fetch(query, n=10); #n is the number of records to be fetched
  dbClearResult(query); #when doing a partial fetch, clear the query from the remote server
  dimmedAffyMisSmall <- dim(affyMisSmall);
  
  dbDisconnect(hg19);
  
  length(allTables)
}

hdf5Notes <- function(){
  
  h5name <- "example.h5";
  created <- h5createFile(h5name)
  created <- h5createGroup(h5name, "foo")
  created <- h5createGroup(h5name, "baa")
  created <- h5createGroup(h5name, "foo/foobaa")
  h5ls(h5name)
  A <- matrix(1:10, nr=5, nc=2)
  h5write(A, h5name, "foo/A") #write the matrix to the group in the file
  B <- array(seq(0.1,2.0,by=0.1), dim=c(5,2,2))
  attr(B, "scale") <- "liter"
  h5write(B, h5name, "foo/foobaa/B")  #write the multi-dimention array with attribute to the group in the file
  
  
  df=data.frame(
    1L:5L, 
    seq(0,1,length.out=5), 
    c("ab","cde","fghi","a","s"), 
    stringsAsFactors=FALSE
  )
  h5write(df, h5name,"df")
  h5ls(h5name)
  
  readA <- h5read(h5name, "foo/A")
  print(readA)
  readB <- h5read(h5name, "foo/foobaa/B")
  print(readB)
  readdf <- h5read(h5name, "df")
}

dataScrapingNotes <-  function(){
  scrapeUrl <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
  authURL <- "http://httpbin.org/basic-auth/user/passwd"
  
  html2 <- GET(scrapeUrl)
  content2 <- content(html2, as="text")
  parsedHtml <- htmlParse(content2, asText=TRUE)
  extractedTitle <- xpathSApply(parsedHtml, "//title", xmlValue)
  #con = url(scrapeURL) worst way of doing this, makes my computer sad
  #htmlCode = readLines(con)
  #close(con)
  #htmlCode
  
  pg2 <- GET(authURL, authenticate("user","passwd"))
  
}

readingApiNotes <- function(){
  twitterURL <- "https://api.twitter.com/1.1/statuses/home_timeline.json"
  # docs for this API https://developer.twitter.com/en/docs/api-reference-index
  myapp <- oauth_app(
    "twitter", 
    key="wC4xsfGjCJUQ0YvQXUdWpi4Ri", 
    secret="JZc6LQtt6q2yVz26XH9s31AcJyrzN4ZkARPRuyhiOBNMQ0njxe"
  )
  sig <- sign_oauth1.0(
    myapp,
    token="17950387-gqSaS3WajoeelbLiia4Ra7ZzEDiwlSwztlz8XIETj",
    token_secret="Wa22DLvwHY5Mano2hmtwv8hofaqtym9yPxkD88rQVJJKB"
  )
  homeTL <- GET(twitterURL, sig)
  jsonRaw <- content(homeTL)
  jsonData <- fromJSON(toJSON(jsonRaw))
  return(jsonData)
}

readingOtherSourcesNotes <- function(){
  twitterURL <- "https://api.twitter.com/1.1/statuses/home_timeline.json"
  # docs for this API https://developer.twitter.com/en/docs/api-reference-index
  myapp <- oauth_app(
    "twitter", 
    key="wC4xsfGjCJUQ0YvQXUdWpi4Ri", 
    secret="JZc6LQtt6q2yVz26XH9s31AcJyrzN4ZkARPRuyhiOBNMQ0njxe"
  )
  sig <- sign_oauth1.0(
    myapp,
    token="17950387-gqSaS3WajoeelbLiia4Ra7ZzEDiwlSwztlz8XIETj",
    token_secret="Wa22DLvwHY5Mano2hmtwv8hofaqtym9yPxkD88rQVJJKB"
  )
  homeTL <- GET(twitterURL, sig)
  jsonRaw <- content(homeTL)
  jsonData <- fromJSON(toJSON(jsonRaw))
  return(jsonData)
}