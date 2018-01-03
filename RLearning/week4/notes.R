#rnorm()  Generates a normal distribution of random numbers rnorm(n, mean = 0, sd = 1)
#pnorm()  evaluates the cumulative distribution for a normal distrubtion
#dnorm()  evaluates the norm probability density with a given mead, at a point (or vector of points)
#rpois()  generate random poisson variates at a given rate

generateMeanTenNumbers <- function(n,sd=1){
    rnorm(n=n, mean=100, sd=sd)
}

plotLinearModel <- function(seed=20, xCount=100, eCount=100){
    set.seed(seed);
    x <- rnorm(xCount); #normal randomized numbers
    e <- rnorm(eCount,0,2);  #normal randomized numbers, different mean and standard deviation
    y <-0.5 + 2 * x + e;
    plot(x,y);
    summary(y);
}

plotBinaryLinearModel <- function(seed=10, xCount=100, eCount=100){
    set.seed(seed);
    x <- rbinom(xCount,1,.5); #normal randomized numbers, binomial
    e <- rnorm(eCount,0,2);  #normal randomized numbers, different mean and standard deviation
    y <-0.5 + 2 * x + e;
    plot(x,y);
    summary(y);
}

plotGeneralizeLinearModelPoisson <- function(){
    set.seed(1);
    x <- rnorm(100);
    log.mu <- .5 + .3 * x;
    y <- rpois(100, exp(log.mu));
    
    plot(x,y);
    summary(y);
}

randomOneToTen <- function(numDesired){
    sample(1:10,numDesired);
}