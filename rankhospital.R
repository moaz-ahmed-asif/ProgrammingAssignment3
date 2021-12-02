rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv")
    ## Check that state and outcome are valid
    states <- unique(dat[, 7])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% states)) {
        stop("invalid state")
    }
    if (!(outcome %in% outcomes)) {
        stop("invalid outcome")
    }
    if (is.numeric(num)) {
        if (length(dat[, 2]) < num) {
            return(NA)
        }
    }
    ## Return hospital name in that state with the given rank
    dat <- subset(dat, State == state)
    
    x <- if (outcome == outcomes[1]) {dat[, c(2, 11)]}
    else if (outcome == outcomes[2])  {dat[, c(2, 17)]}
    else {dat[, c(2, 23)]}
    suppressWarnings(x[, 2] <- as.numeric(x[, 2]) )
    x <- subset(x, !is.na(x[, 2]))
    x <- x[order(x[, 2], x[, 1]), 1]
    
    x <- if (num=="best")  {head(x, 1)}
    else if (num=="worst") {tail(x, 1)}
    else x[num]
    
    x
}
