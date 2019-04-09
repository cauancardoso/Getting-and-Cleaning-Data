rankhospital <- function(state, outcome, num = "best") {


	## Read outcome data


	if(!exists("outcome_of_care")) { 
		outcome_of_care <<- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE,
		colClasses = c("NULL", NA, # Hospital name
		rep("NULL",4), NA, # State
		rep("NULL", 3), NA, # Heart attack rate
		rep("NULL", 5), NA, # Heart failure rate
		rep("NULL", 5), NA, # Pneumonia rate
		rep("NULL", 23))
		)
	}


	## Check that state and outcome are valid

	
	# State

	if(nrow(outcome_of_care[outcome_of_care$State == state, ]) == 0) {
		stop("invalid state")
		}

	# Outcome

	valid_outcomes = c("heart attack", "heart failure", "pneumonia")
	if(!(outcome %in% valid_outcomes)) {
		stop("invalid outcome")
	}


	## Return hospital name in that state with the given rank 30-day death rate


	# Object that defines the number of the column to be analyzed based on outcome argument

	outcomes <- list("heart attack"	= 3, 
			"heart failure"	= 4, 
			"pneumonia"	= 5
			)

	# Return the lowest 30-day death rate from outcome in a specified state (num = "best")
	
	if(num == "best") {	
	a <- outcome_of_care[ which(outcome_of_care$State == state), c(1, outcomes[[outcome]]) ]
	b <- a[ order(a[,2], a[,1]), ]
	result <- b[1,1]
	print(result)
	}

	# Return the highest 30-day death rate from outcome in a specified state (num = "worst")

	else if(num == "worst") {	
	a <- outcome_of_care[ which(outcome_of_care$State == state), c(1, outcomes[[outcome]]) ]
	b <- a[ order(a[,2], a[,1], decreasing = TRUE), ]
	result <- b[1,1]
	print(result)
	}
	
	# Return the nth 30-day death rate from outcome in a specified state (num = numeric)

	else if(class(num) == "numeric") {	
	a <- outcome_of_care[ which(outcome_of_care$State == state), c(1, outcomes[[outcome]]) ]
	b <- a[ order(a[,2], a[,1]), ]
	result <- b[num,1]
	print(result)
	}
	
}