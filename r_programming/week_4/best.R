best <- function(state, outcome) {
    # Determines the best hospital in a state for a certain condition.
    #
    # Args:
    #    state: A character vector of length 1 that is the two letter
    #           abbreviation for the state of interest (e.g., "PA").
    #    outcome: A character vector of length 1. Possible conditions are 
    #           "heart attack", "heart failure", and "pneumonia".
    # Returns:
    #    A character vector indicating the best hospital in a state a 
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
        winner <- state_results[which(state_results$heart_attack == 
                                      min(state_results$heart_attack, 
                                          na.rm = TRUE)),]
        return(winner$hospital)
    }
    if(outcome == "heart failure") {
        winner <- state_results[which(state_results$heart_failure == 
                                      min(state_results$heart_failure, 
                                          na.rm = TRUE)),]
        return(winner$hospital)
    }
    if(outcome == "pneumonia") {
        winner <- state_results[which(state_results$pneumonia == 
                                      min(state_results$pneumonia, 
                                      na.rm = TRUE)),]
        return(winner$hospital)
    }
}