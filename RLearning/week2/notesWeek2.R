BasicForLoopExample <- function(){
    x <- c("a", "b", "c", "d")
    for(i in 1:4){
        print(x[i])
    }
    
    for(i in seq_along(x)){
        print(x[i])
    }
    
    for(letter in x){
        print(letter)
    }
}


matrixForLoopExample <- function(){
    x <- matrix(1:6, 2, 3)
    
    for(i in seq_len(nrow(x))){
        for(j in seq_len(ncol(x))){
            cat('\n','X index: ', i, "Y index: ", j, " value: ", x[i,j])
            #print( ", " + j + ": " + x[i,j])
        }
    }
}

nextInForLoopExample <- function(){
    for(i in 1:100){
        if(i%%3==0) next()
        cat("HI! " ,i , " ")
    }
    
    for(i in seq_along(x)){
        print(x[i])
    }
    
    for(letter in x){
        print(letter)
    }
}

whileLoopExample <- function(){
    count <- 0;
    while(count < 10){
        cat(count)
        count <- count + 1;
    }
}

repeatLoopExample <- function(){
    x <- 1;
    tol <- 100;
    repeat {
        x<- x+1;
        if(x>tol) break;
        if(x%%5 == 0) cat(x," ");
    }
}

removeValuesBelowMatrix <- function(matrix, cutoff = 10){
    stopifnot(is.matrix(matrix));
    use <- matrix > cutoff;
    x <- matrix[use];
    return(x);
}

columnMean <- function(y, removeNA= TRUE){
    stopifnot(is.matrix(y));
    nc <- ncol(y);
    means <- numeric(nc);
    for(i in i:nc) means[i] <- mean(y[, i], na.rm = removeNA);
    return(means);
}

cube <- function(x, n) {
    x^3
}

test <- function(){
    x <- 1:10
    if(x > 5) {
        x <- 0
    }
}

f <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)
}







