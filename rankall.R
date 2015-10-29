rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        ## Check that state and outcome are valid
        if(outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE){        
                stop("invalid outcome")
        }
        
        validState = sort(unique(outcome_data[,7]))
        #if (!state %in% validState) stop("invalid state")
        ## Figure out what outcome is being measured
        if (outcome == "heart attack"){
                outcomeCol <- 11
        } else if (outcome == "heart failure") {
                outcomeCol <- 17
        } else if (outcome == "pneumonia") {
                outcomeCol <- 23
        }
        
        data <- outcome_data[c(2, 7, outcomeCol)] ## extract only the columns we need
        
        names(data)[1] <- "hospital"
        names(data)[2] <- "st"
        names(data)[3] <- outcome
        
        ## For each state, find the hospital of the given rank
        hospital <- character(0L)
        
        for (i in seq_along(validState)) {
                data.state <- data[data$"st" == validState[i],]
                
                sorted.data.state <- data.state[order(as.numeric(data.state[[outcome]]), data.state[["hospital"]], decreasing = FALSE, na.last = NA), ]
                
                #handle num input
                if (num == "best") {
                        num_i <- 1
                }
                if (num == "worst") {
                        num_i <- nrow(sorted.data.state)
                }
                hospital[i] <- sorted.data.state[num_i, "hospital"]
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        data.frame(hospital = hospital, state = validState, row.names = validState)
}
