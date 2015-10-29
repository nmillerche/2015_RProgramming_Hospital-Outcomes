rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        ## Check that state and outcome are valid

        if(outcome %in% c("heart attack", "heart failure", "pneumonia") == FALSE){        
                stop("invalid outcome")
        }
        if(state %in% unique(outcome_data[, "State"]) == FALSE){
                stop("invalid state")       
        }        
        
        
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        data.state <- data[data$"st" == state,]
        
        sorted.data.state <- data.state[order(as.numeric(data.state[[outcome]]), data.state[["hospital"]], decreasing = FALSE, na.last = NA), ]
        
        ##handle num input
        if (num == "best") {
                num <- 1
        }
        if (num == "worst") {
                num <- nrow(sorted.data.state)
        }
        sorted.data.state[num,"hospital"]
}
