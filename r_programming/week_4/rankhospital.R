rankhospital <- function(state, outcome, num = "best") {
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