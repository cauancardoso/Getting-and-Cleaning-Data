rankall <- function(outcome, num = "best") {


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


	## Check that outcome is valid

	
	# Outcome

	valid_outcomes = c("heart attack", "heart failure", "pneumonia")
	if(!(outcome %in% valid_outcomes)) {
		stop("invalid outcome")
	}


	## For each state, find the hospital of the given rank


	# Object that defines the number of the column to be analyzed based on outcome argument

	outcomes <- list("heart attack"	= 3, 
			"heart failure"	= 4, 
			"pneumonia"	= 5
			)

	# Object that contains a list of the hospitals by states ordered from lowest 30-day death rate from outcome to highest

	# Return the lowest 30-day death rate from outcome in a specified state (num = "best")

	if(num == "best") {

	a <- outcome_of_care[, c(1, 2, outcomes[[outcome]]) ]
	b <- a[ order(a[,2], a[,3], a[,1]),]
	split.outcome.by.state <- split(b[,c(-2,-3)], b$State)

	my.list <- lapply(split.outcome.by.state,  "[", 1)
	my.list.names <- names(my.list)
	my.list.values <- unlist(my.list, use.names = FALSE)
	result <- data.frame(hospital = my.list.values, state = my.list.names, row.names = my.list.names)
	result
	}

	# Return the highest 30-day death rate from outcome in a specified state (num = "worst")

	else if(num == "worst") {	

	a <- outcome_of_care[, c(1, 2, outcomes[[outcome]]) ]
	b <- a[ order(a[,2], a[,3], a[,1], decreasing = TRUE),]
	split.outcome.by.state <- split(b[,c(-2,-3)], b$State)

	my.list <- lapply(split.outcome.by.state,  "[", 1)
	my.list.names <- names(my.list)
	my.list.values <- unlist(my.list, use.names = FALSE)
	result <- data.frame(hospital = my.list.values, state = my.list.names, row.names = my.list.names)
	result
	}
	
	# Return the nth 30-day death rate from outcome in a specified state (num = numeric)

	else if(class(num) == "numeric") {	

	a <- outcome_of_care[, c(1, 2, outcomes[[outcome]]) ]
	b <- a[ order(a[,2], a[,3], a[,1]),]
	split.outcome.by.state <- split(b[,c(-2,-3)], b$State)

	my.list <- lapply(split.outcome.by.state,  "[", num)
	my.list.names <- names(my.list)
	my.list.values <- unlist(my.list, use.names = FALSE)
	result <- data.frame(hospital = my.list.values, state = my.list.names, row.names = my.list.names)
	result
	}
	
}