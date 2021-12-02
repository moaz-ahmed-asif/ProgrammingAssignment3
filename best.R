best <- function(state, outcome) {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    states <- unique(dat[, 7])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% states)) {
        stop("invalid state")
    }
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death
    dat <- subset(dat, State == state)
    
    x <- if (outcome == outcomes[1]) {dat[, c(2, 11)]}
    else if (outcome == outcomes[2])  {dat[, c(2, 17)]}
    else {dat[, c(2, 23)]}
    suppressWarnings(x[, 2] <- as.numeric(x[, 2]) )
    x <- subset(x, !is.na(x[, 2]))
    
    head(x[order(x[, 2],x[, 1]), 1], 1)
}
