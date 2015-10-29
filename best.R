best <- function(state, outcome) {
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data <- outcome_data[c(2, 7, 11, 17, 23)] ## extract only the columns we need
        names(data)[1] <- "hospital"
        names(data)[2] <- "st"
        names(data)[3] <- "heart attack"
        names(data)[4] <- "heart failure"
        names(data)[5] <- "pneumonia"
        
        data[, "heart attack"] <- as.numeric(data[, "heart attack"])
        data[, "heart failure"] <- as.numeric(data[, "heart failure"])
        data[, "pneumonia"] <- as.numeric(data[, "pneumonia"])
        
        ## Check that state and outcome are valid
        state_list <- unique(data$st)
        outcome_list <- c("heart attack", "heart failure", "pneumonia")
        
        
        if(state %in% state_list == FALSE){
                stop("invalid state")       
        }        
        
        if(outcome %in% outcome_list == FALSE){
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day 
        ## death rate
        
        statehospitals <- data[which(data[, 2] == state), "hospital"]
        hospital_outcomes <- data[which(data[, 2] == state), outcome]
        rownum <- which.min(hospital_outcomes)
        statehospitals[rownum]
}
