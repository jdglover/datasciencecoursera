rankall <- function(outcome, num = "best") {
    # Determines the Nth ranked hospital for a given condition across all
    # states.
    #
    # Args: 
    #    outcome: A character vector of length 1. Possible conditions are 
    #           "heart attack", "heart failure", and "pneumonia".
    #    num: The ranked hospital. Possible values are "best', "worst", or
    #         a numerical ranking (e.g., 5).
    #
    # Returns:
    #    A data frame indicating the Nth ranked hospital across all states
    #    for a particular outcome.
    data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    poss_states <- unique(data$State)
    poss_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if(is.na(match(outcome, poss_outcomes))) {
        stop("invalid outcome")
    }
    
    relevant_data <- data.frame(data[,2], data[,7], data[,11], 
                                data[,17], data[,23], stringsAsFactors=FALSE)
    colnames(relevant_data) <- c("hospital", "hospital_state", "heart_attack", 
                                 "heart_failure", "pneumonia")
    
    relevant_data[,3] <- as.numeric(as.character(relevant_data[,3]))
    relevant_data[,4] <- as.numeric(as.character(relevant_data[,4]))
    relevant_data[,5] <- as.numeric(as.character(relevant_data[,5]))
    
    if(outcome == "heart attack") {
        split_data <- split(relevant_data, relevant_data$hospital_state)
        split_outcome <- lapply(split_data, 
                                function(x) na.omit(relevant_data[order(
                                    relevant_data$hospital_state, 
                                    relevant_data$heart_failure,
                                    relevant_data$hospital),]))
        
    }
    if(outcome == "heart failure") {
        split_data <- na.omit(relevant_data[order(relevant_data$hospital_state,
                                              relevant_data$heart_failure,
                                              relevant_data$hospital),])
    }
    if(outcome == "pneumonia") {
        split_data <- na.omit(relevant_data[order(relevant_data$hospital_state,
                                              relevant_data$pneumonia,
                                              relevant_data$hospital),])
    }
    
    if (num == "best") {
        return(str(split_outcome))
        #return(lapply(split_outcome, head(split_outcome)[1,1]))
        } else if (num == "worst") {
        return(tail(winner)[6,1])
    } else {
        return (winner[num,1])
    }
}