library("lubridate")
# Start time Jan 3rd 2018 @ 2020
textVariableManipulationNotes <- function(){
  # want all lower case data
  # descriptive text/variable names
  # not duplicated
  # no underscores or dots or extra whitespace
  # should be descriptive MALE vs M
  # show usually be made into factors if possible
  
  fileName <- "./data/cameras.csv"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName)){
    fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
    download.file(fileUrl,destfile = fileName)
  }
  cameraData <- read.csv(fileName)
  print("here we can look at the column names")
  print(names(cameraData))
  print("You can use tolower to lowercase all characters")
  print(tolower(names(cameraData)))
  print("You can also split out values based on criteria, example: not wanting \\. in a filename")
  splitNames <- strsplit(names(cameraData),"\\.")
  print(splitNames)
  print("removing weird names can also be done with sapply")
  firstElement <- function(x){x[1]}
  sapply(splitNames, firstElement)
  print(splitNames)
  mylist <- list(letters = c("A","b","c"), numbers=1:3, matrix(1:25, ncol=5))
  print(head(mylist))
  print("You can also use sub to remove unwanted characters")
  testName <- "this_is_a_test"
  testName <- sub("_","",testName)
  print(testName)
  print("and subg to remove all of them")
  testName2 <- "test_time_is_over_dumby"
  testName2 <- gsub("_"," ", testName2)
  print(testName2)
  
  print("you may want to find cameras on your street")
  results <- grep("Alameda",cameraData$intersection)
  print(results)
  print("Or find out how proportional it is with grepl (returns an array of bools representing where it was found)")
  print(table(grepl("Alameda",cameraData$intersection)))
  cameraData2 <- cameraData[!grepl("Alameda", cameraData$intersection),]
  print(head(cameraData2, n=5))
  print("Or get a list of results for your street")
  results <- grep("Alameda",cameraData$intersection, value=TRUE)
  print(results)
  
  print("You can get substrings")
  print(substr("Paul Sabatino",1,4))
  print("Or combine strings with paste")
  print(paste("Paul","Sabatino"))
}

regexManipulationNotes <- function(){
  # ^ marks the start of a line
  #   ^I Think so
  #   I Think so (match)
  #   Don't I think so? (No match)
  # $ marks the end of the line
  #   I doubt it$
  #   He said, she said, I doubt it (match)
  #   I doubt it! (no match)
  # [] Will make a list of characters acceptable, including range [0-9][a-z][A-Z][a-zA-Z] as examples
  # Inside [] the ^ negates the acceptable range
  #   [^?.]$ says lines that don't end with ? or .
  # . means any character
  #   so 9.11
  #     is looking for something like 9T11
  # | is the or operator
  #   flood|fire
  #     looks for either fire or flood in a line
  # alternatives can also be there own regex
  #   ^[Gg]ood|[Bbad]
  #     looks for either good a the beginning of the line, or bad anywhere
  # parentheses give you alternatives that obey outer conditions
  #   ^([Gg]ood|[Bbad])
  #     looks for good or bad at the beginning of the line
  # next to () the ? is an optional
  #   George( [Ww]\.)? Bush
  #     looks for george bush with or without a W. w.
  #     note the escape (the \) . is a special character so to use it as a literal it must be escaped
  # the * sign is look for the proceeding character any number of times
  #   (.*)
  #     look for any character any number of times
  # the + sign says at least 1 must exist
  #   [0-9]+
  #     at least 1 number
  # {num, num} lets you set the number of matches you want
  #   (a|b){1,5}
  #     look for 1 to 5 a or b characters in a row
  # {num} look for exactly this number of matches
  # {num, } look for at least this number of matches
  # the \num will look for repeats of the previous evaluation
  #   +([a-zA-Z]+) +\1 +
  #     time for bed, night night twitter (matches)
  # * is greedy, will match on the longest possible string that satisfies it, unless turned off with ?
}

workingWithDatesNotes <- function(){
  d1 <- date()
  print(d1)
  print(paste("d1 class",class(d1)))
  d2 <- Sys.Date()
  print(d2)
  print(paste("d2 class",class(d2)))
  # use format to format dates, %d is day as a number; %a is abbreviated weekday; %A is unabbreviated weekday;
  # %m is month; %B is unabbreviated month; %y is the 2 digit year; %Y is the 4 digit year
  print(format(d2,"%d%b%Y"))
  
  # you can create dates in a vector or list with as.Date
  x <- c("1jan1960","31mar1960")
  z <- as.Date(x, "%d%b%Y")
  print(z)
  print(z[1] - z[2])
  print(as.numeric(z[1]-z[2]))
  
  # the lubridate library is awesome for date manipulation
  print(lubridate::ymd("20140108"))
  print(lubridate::dmy("08012014"))
  
  # it also works for time
  print(ymd_hms("2011-09-03 10:15:03")) # you can use tz with tihs to set time zones
  # you learn more with r-statistics.com/2012/03/do-more-with-dates-and-times-in-r-with-lubridate-1-1-0/
  # wday is the same as weekday in the standard date package
}

freeDataSources <- function(){
  # data.un.org
  # data.gov
  # data.gov.uk
  # data.gouv.uk
  # data.gov.gh
  # data.gov.au
  # gapminder.org
  # http://asdfree.com
  # infochimps.com/marketplace
  # kaggle.com (DS competitions)
  # twitteR package
  # rfigshare package
  # RFacebook package
  # rOpenSci package
}
