
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

#outcome[, 11] <- as.numeric(outcome[, 11])
#hist(outcome[, 11])

best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    y <- with(outcome[outcome$State == state,], tapply(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,mean))
    
}