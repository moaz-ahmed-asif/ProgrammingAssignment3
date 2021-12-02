rankall <- function(outcome, num = "best") {
    ## Read outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (!(num=="best" || num=="worst" || is.numeric(num))) stop("invalid num")
    states <- as.data.frame(unique(dat[, 7]))
    colnames(states) <- "State"
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    x <- if (outcome == outcomes[1]) {dat[, c(2, 7, 11)]}
    else if (outcome == outcomes[2])  {dat[, c(2, 7, 17)]}
    else {dat[, c(2, 7, 23)]}
    suppressWarnings(x[, 3] <- as.numeric(x[, 3]) )
    x <- subset(x, !is.na(x[, 3]))
    y <- x[order(x[, 2], x[, 1]), ]
    y$order <- unlist(with(y, tapply(y[, 3], y[, 2], 
                               function(z) rank(z, ties.method="first"))))
    number <- if (num == "best") {1} else if (num == "worst") {40} else {num}
    
    merge(states, subset(y, y$order == number), 
          by.x = "State", by.y = "State", all.x=TRUE)[, c(2, 1)]
}
