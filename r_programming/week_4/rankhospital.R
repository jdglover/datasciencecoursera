rankhospital <- function(state, outcome, num = "best") {
    # Determines the Nth ranked hospital for a given condition in a given state.
    #
    # Args: 
    #    state: A character vector of length 1 that is the two letter
    #           abbreviation for the state of interest (e.g., "PA").
    #    outcome: A character vector of length 1. Possible conditions are 
    #           "heart attack", "heart failure", and "pneumonia".
    #    num: The ranked hospital. Possible values are "best', "worst", or
    #         a numerical ranking (e.g., 5).
    #
    # Returns:
    #    A character vector indicating the Nth ranked hospital in a state for a 
    #    particular outcome.
    data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    poss_states <- unique(data$State)
    poss_outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if(is.na(match(state, poss_states))) {
        stop("invalid state")
    }
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
    
    state_results <- subset.data.frame(relevant_data, hospital_state==state)
    
    if(outcome == "heart attack") {
        winner <- na.omit(state_results[order(state_results$heart_attack,
                                              state_results$hospital),])
    }
    if(outcome == "heart failure") {
        winner <- na.omit(state_results[order(state_results$heart_failure,
                                              state_results$hospital),])
    }
    if(outcome == "pneumonia") {
        winner <- na.omit(state_results[order(state_results$pneumonia,
                                              state_results$hospital),])
    }
    
    if (num == "best") {
        return(head(winner)[1,1])
    } else if (num == "worst") {
        return(tail(winner)[6,1])
    } else {
        return (winner[num,1])
    }
}