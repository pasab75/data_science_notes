library(stringr)

#dataset_url = "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip";
#download.file(dataset_url, "pollutionData.zip");
#unzip("pollutionData.zip", exdir = "pollutionData");

csvNamePadding <- function(num, padLength=3, paddingChar="0"){
    return(paste0(trimws(str_pad(num,3,pad="0")),".csv"));
}

pollutantmean <- function(directory, pollutant, ids =1:332){
    values <- c();
    
    for(id in ids){
        csvName <- csvNamePadding(id);
        #cat(csvName, "\n");
        file <- read.csv(paste0(directory,'/',csvName));
        value <- file[[pollutant]];
        #print(value[!is.na(value)]);
        values <- append(values,value[!is.na(value)]);
    }
    #print(values);
    m <- mean(values);
    return(m);
}

completeInCSV <- function(directory, id){
    csvName <- csvNamePadding(id);
    #cat(csvName, "\n");
    file <- read.csv(paste0(directory,'/',csvName));
    completeCases <- file[complete.cases(file), ];
    return(completeCases)
}

complete <- function(directory, ids = 1:332){
    nobs <- unlist(
        lapply(ids, function(id) nrow(
                completeInCSV(directory, id)
            )
        )
    );
    #for(id in ids){
    #    
    #    cases[i] <- 
    #    print(cases[i]);
    #}
    #print(unlist(cases));
    
    x <- data.frame(ids, nobs)
    print(x);
}


coorLoad <- function(directory, id){
    csvName <- csvNamePadding(id);
    file <- read.csv(paste0(directory,'/',csvName));
    return(cor(file$sulfate, file$nitrate, use="complete.obs"));
}

corr <- function(directory, threshold=0, ids=1:332){
    comp <- complete(directory = directory, ids = ids);
    #complete <- complete[nrow(complete) > threshold];
    goodSamples <- apply(comp, 1, function(x){ print(x[[2]]); x[[2]] > threshold});
    goodIds <- ids[goodSamples];
    #print(goodIds);
    correlation_data <- unlist(lapply(goodIds, function(x) coorLoad(directory, x)));
    return(correlation_data);
}

