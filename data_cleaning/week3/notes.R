library("plyr")
library("Hmisc")
# Working times
# Dec 26th 2230 -> 2330
# Dec 27th 1400 -> 1630 



subsettingNotes <- function(){
  X <- data.frame("var1"=sample(1:5), "var2"=sample(6:10), "var3"=sample(11:15))
  X <- X[sample(1:5),]
  print(X)
  # selection examples
  
  # select a column examples
  print(X[,1])
  print(X[,"var1"])
  
  #row and column selections, first 2 rows, second column
  print(X[1:2, "var2"])
  
  # using logical selection, where var1 in X is less than or equal to 3 and 
  # where var3 within X is greater than 11
  print(X[X$var1 <=3 & X$var3 > 11,])
  
  # using logical selection, where var1 in X is less than or equal to 4 or 
  # where var3 within X is greater than 13
  print(X[X$var1 <=4 | X$var3 > 13,])
  
  #you can use sort on a single column
  print(sort(X$var1))
  
  print(sort(X$var1, decreasing=TRUE))
  print(sort(X$var2, na.last=TRUE))
  
  #showing the arrange feature of plyr
  print(arrange(X,var1))
  print(arrange(X, desc(var1)))
  
  #adding columns
  X$var4 <- rnorm(5)
  X <- cbind(X, rnorm(5))
  print(X)
}

summarizingData <- function(){
  fileName <- "./data/restaurants.csv"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName)){
    fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
    download.file(fileUrl,destfile = fileName)
  }
  restData <- read.csv(fileName)
  #head shows the top n rows of the data frame
  print(head(restData,n=3))
  #tail shows the bottom n rows of the data frame
  print(tail(restData,n=3))
  
  #summary shows us some quick facts about the dataframe
  print(summary(restData))
  
  #find the basic info on the variable
  print(str(restData))
  
  #quantile gives statistic data on the dataframe
  print(quantile(restData$councilDistrict,na.rm=TRUE))
  print(quantile(restData$councilDistrict, probs=c(.5,.75,.9)))
  
  # table lets us look at specific data, and use NA is a useful command:
  # if any missing values, na will be a value in the table letting you know how many were missing
  zipCodes <- table(restData$zipCode, useNA="ifany")
  print(zipCodes)
  
  # you can make 2D tables
  twoDimensionalTable <- table(restData$councilDistrict, restData$zipCode)
  print(twoDimensionalTable)
  # to find NA in this 2d table you'll need to sum the number and report it
  print("number NA in columns: ")
  print(sum(is.na(restData$councilDistrict)))
  # any can check for presense
  print("any NA in columns? ")
  print(any(is.na(restData$councilDistrict)))
  # any can check for presense
  print("do all variables statisfy the condition in columns (are all zipcodes greater than 0)? ")
  print(all(restData$zipCode > 0))
  
  # sum columns
  print("number of NA in data set")
  print(colSums(is.na(restData)))
  
  # find all zipcodes equal to 21212, using the table in command
  specificZipCode <- table(restData$zipCode %in% c("21212"))
  print(specificZipCode)
  
  specificZipCodes <- table(restData$zipCode %in% c("21212","21213"))
  print(specificZipCodes)
  
  # you can subset using %in% as a logical operator
  # print(restData[restData$zipCode %in% c("21212", "21213"),])
  
  #you can use xtabs to create relationships between variables
  
  # to messure dataset size you can, you can also cast it to specific units
  print(object.size(restData))
  print(object.size(restData), units="Mb")
  
  #make sequences, give min, max, and then how to index it
  # one way is the "by={x}" argument, where x is the incrementor so if x was 2, and we started at min 1
  # it would give the sequence 1, 3, 5, 7, 9, etc.
  # the other way is "length={x}", this will give x values starting with min and ending with max
  # the last way is to take a vector and do seq(along={Vector})
  s1 <- seq(1,10,by=2)
  s2 <- seq(1,10,length=3)
  X <- c(1,3,8,25,100)
  s3 <- seq(along=X)
  
  restData$nearMe <- restData$neighborhood %in% c("Roland Park", "Homeland")
  print(head(restData, n=5))
  #sequencing can be useful for 
  restData$zipWrong <- ifelse(restData$zipCode <0, TRUE, FALSE)
  print(head(restData, n=5))
  
  # you can use cut to create intervals on the "break={numericVector || numberOfIntervalsNeeded}"
  restData$zipGroups <- cut(restData$zipCode, breaks=quantile(restData$zipCode))
  print(head(restData, n=5))
  
  #Hmisc is a handy library that can do quantile cutting even easier, it has the cut2 command
  restData$zipGroups <- cut2(restData$zipCode,g=4)
  print(head(restData, n=5))
  
  #factor variable can be created from normal variables with factor
  restData$zcf <- factor(restData$zipCode)
  print(head(restData, n=5))
  # if you want to see this as enums, you can call as.numeric on the factor
  # as an example here as.numeric(restData$zcf) would give us numeric values for it
  print(head(as.numeric(restData$zcf)))
  
  #mutate from plyr creates a new variable and add it to the existing dataframe
  restData2 <- mutate(restData, zipGroups=cut2(restData$zipCode, g=4))
  
  #common transforms
  # abs(x) absolute value
  # sqrt(x) sqrt
  # ceiling(x) round up values to integers
  # floor(x) round down values to integers
  # round(x, digits=n) round values to the Xth digit of precision
  # signif(x, digits=n) round values to the (X-1)th digit of precision
  # cos(x), sin(x), tan(x), etc.
  # log(x) natural log of X
  # log2(x), log10(x), etc.
  # exp(x) exponentiating of X
}

reshapingData <- function(){
  # how to make tidy data
  # each variable has a column
  # each observation has its own row
  
  library(reshape2)
  # included in R dataset mtcars
  
  #melting a dataset
  mtcars$carNames <- rownames(mtcars)
  carMelt <- melt(mtcars, id=c("carNames", "gear", "cyl"), measure.vars=c("mpg","hp"))
  print(head(carMelt,n=3))
  
  # dcast does this by length if the 3rd param is not provided
  cylData <- dcast(carMelt, cyl ~ variable)
  print(head(cylData, n=5))
  
  #here we're specifying to use mean
  cylData <- dcast(carMelt, cyl ~ variable, mean)
  print(head(cylData, n=5))
  
  # this will sum each cyl types horsepower to get the average
  print(tapply(cylData$cyl, cylData$hp, sum))
  
  # another way to do this is to split, apply, then combine
  spCyl <- split(cylData$cyl, cylData$hp)
  hpPerCyl <- lapply(spCyl, sum)
  vectHpPerCyl <- unlist(hpPerCyl)
  print(head(vectHpPerCyl, n=5))
  #remember sapply simplifies its output so another way to do this (simpler)
  vectHpPerCyl <- sapply(spCyl, sum)
  print(head(vectHpPerCyl, n=5))
  
  #yet another way, the ddply
  ddply(cylData,.(cyl),summarize,sum=sum(hp))
}

dplyNotes <- function(){
  #dplyr assumes the same things as tidy data:
  # each variable has a column
  # each observation has its own row
  fileName <- "./data/chicago.RDS"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName)){
    fileURL <- "https://github.com/DataScienceSpecialization/courses/blob/master/03_GettingData/dplyr/chicago.rds?raw=true"
    download.file(fileURL,destfile = fileName)
  }
  chicago <- readRDS(fileName)
  
  # lets get the column names to manipulate this dataset with
  print(names(chicago))
  
  #dplyr functions
  # arrange: reorder rows
  chicago <- arrange(chicago, desc(date))
  print(head(chicago))
  
  chicago <- arrange(chicago, date)
  print(head(chicago))
  
  # filter: extract a subset of rows from the data frame based on conditionals
  #    args: dataframe, conditional statement
  filtered <- filter(chicago, pm25tmean2 > 30)
  print(head(filtered > 10))
  
  # rename: rename variables in a dataframe
  chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp)
  print(head(chicago))
  
  # mutate: add new variables/columns to a dataframe or transform existing variables
  chicago <- mutate(chicago, pm25detrend=pm25-mean(pm25, na.rm=TRUE))
  print(head(chicago))
  chicago <- mutate(chicago, tempcat = factor(1 * (tmpd > 80), labels =c("cold", "hot")))
  
  # select: return a subset of the columns of the data frame
  #   lets say I want to look at the columns between city and dptp I could use the following command
  subset <- select(chicago, city:dewpoint)
  print(head(subset))
  #you can also exclude all but with
  subset <- select(chicago, -(city:dewpoint))
  print(head(subset))
  
  # summarize: generate summary statistics of different variables in a data frame
  hotcold <- group_by(chicago, tempcat)
  print(head(hotcold))
  summary <- summarize(hotcold, pm25 = mean(pm25, na.rm=TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))
  print(head(summary))
  
  #   you can also sum per year
  chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
  years <- group_by(chicago, year)
  year_summary <- summarize(years, pm25 = mean(pm25, na.rm=TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))
  print(head(year_summary))
  
  # %>% is the pipeline operator, applying the variable given/output as the first arg in the next function
  chicago %>% mutate(month = as.POSIXlt(date)$mon + 1) %>% group_by(month) %>% summarize(pm25 =mean(pm25, na.rm=TRUE), o3=max(o3tmean2), no2 = median(no2tmean2))

}

mergingData <- function(){
  fileName1 <- "./data/reviews.csv"
  fileName2 <- "./data/solutions.csv"
  if(!file.exists("./data")){dir.create(("./data"))}
  if(!file.exists(fileName1)){
    fileUrl <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
    download.file(fileUrl,destfile = fileName1)
  }
  if(!file.exists(fileName2)){
    fileUrl <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
    download.file(fileUrl,destfile = fileName2)
  }
  
  reviews <- read.csv("./data/reviews.csv");
  solutions <- read.csv("./data/solutions.csv");
  print(names(reviews))
  print(names(solutions))
  
  # merge important params, x.y, by, by.y, by.x, all (defaults all)
  mergedData <- merge(reviews, solutions, by.x="solution_id", by.y="id", all=TRUE)
  print(head(mergedData))
  
  # by default merge will do this on the intersection
  # intersect(reviews, solutions)
  
  #using join in plyr
  df1 <- data.frame(id=sample(1:10), x=rnorm(10))
  df2 <- data.frame(id=sample(1:10), y=rnorm(10))
  df3 <- data.frame(id=sample(1:10), z=rnorm(10))
  joined_data <- join(df1, df2)
  dfList <- list(df1, df2, df3)
  joined_all <- join_all(dfList)
}