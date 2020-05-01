## the best function
best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        ## Make sure that the state value exits
        states <- data[ ,7]
        states1 <- as.character(states)
        state_value <- sum(states1 == state)
        if(state_value == 0) {
                stop("invalid state")    ##stop, if state doesn't exist
        } else {
                state_value <- TRUE
        }
        ## Make sure that the disease value is valid
        if(outcome == "heart attack"){
                death_rate <- as.numeric(levels(data[ , 11])[data[ ,11]])
        } else if (outcome == "heart failure"){
                death_rate <- as.numeric(levels(data[ , 17])[data[ ,17]])
        } else if (outcome == "pneumonia"){
                death_rate <- as.numeric(levels(data[ , 23])[data[ ,23]])
        } else{
                stop("invalid outcome")  ##stop, if the disease if invalid 
        }
        ## combine the hospital name, states, death rate to a list
        hospital <- as.character(data[ ,2])
        best_data_total<- data.frame(states1, hospital, death_rate)
        best_data <- best_data_total[states1 == state, ]
        order_data1 <- best_data[order(best_data[ ,3],na.last = NA), ]
        order_data1 [1, 2]
        
}
        
        
        
        
        
     