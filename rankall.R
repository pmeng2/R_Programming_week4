## the rankhospital function
rankall <- function(outcome, num) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv")
        states <- data[ ,7]
        states1<- as.character(states)
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
        data_total<- data.frame(states1, hospital, death_rate)
        data_split<- split(data_total, data_total$states1)
        hospital <- c()
        for (i in 1:length(data_split)){
                data_i <- as.data.frame(data_split[i])
                order_datai <- data_i[order(data_i[ ,3], data_i[ ,2], 
                                            na.last = NA), ]
                if(num =="best"){
                        hos_i<- as.character(order_datai[1,2])
                }
                else if (num == "worst"){
                        hos_i<- as.character(order_datai[nrow(order_datai), 2])
                }
                else{ 
                        hos_i<- as.character(order_datai[num, 2])
                }
                hospital <- c(hospital, hos_i)
        }
        state<- c(levels(states))
        results<- data.frame(hospital, state)
        results
}
        
        
        
        
        
     