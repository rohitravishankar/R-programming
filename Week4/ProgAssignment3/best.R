best <- function(state, outcome) {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
## rate

	directory <- "outcome-of-care-measures.csv"
	data <- read.csv(directory, colClasses="character")

	data[, 11] <- as.numeric(data[, 11]) # heart attack
    	data[, 17] <- as.numeric(data[, 17]) # heart failure
    	data[, 23] <- as.numeric(data[, 23]) # pneumonia
	if (!state %in% data$State) {
        stop("invalid state")
   	} else if(!outcome %in% valid_outcomes) {
        stop("invalid outcome")
    	} else {
		if(outcome == "heart attack")
		{
			#stores all data corresponding to that state
			state_subset <- data[data[, 7]==state, ] 

			#finds the minimum value corresponding to minimum heart attack rates in that state
   			min <- min(state_subset[, 11], na.rm=T)

			#finds the index corresponding to the minimum value
	    		min_index <- which(state_subset[, 11] == min)	

			#finds hospital name corresponding to the minimum index
   			hosp_name <- state_subset[min_index, 2]

	      } else if(outcome == "heart failure") {

			#stores all data corresponding to that state
			state_subset <- data[data[, 7]==state, ]
	
			#finds the minimum value corresponding to minimum heart attack rates in that state
	   		min <- min(state_subset[, 17], na.rm=T)

			#finds the index corresponding to the minimum value
	    		min_index <- which(state_subset[, 17] == min)
	
			#finds hospital name corresponding to the minimum index
	   		hosp_name <- state_subset[min_index, 2]
	
	      } else if(outcome == "pneumonia")	{
	
			#stores all data corresponding to that state
			state_subset <- data[data[, 7]==state, ]
	
			#finds the minimum value corresponding to minimum heart attack rates in that state
	   		min <- min(state_subset[, 23], na.rm=T)
	
			#finds the index corresponding to the minimum value
	    		min_index <- which(state_subset[, 23] == min)
	
			#finds hospital name corresponding to the minimum index
	   		hosp_name <- state_subset[min_index, 2]
	      }
      	result <- hosp_name
	      return(result)
	}

}