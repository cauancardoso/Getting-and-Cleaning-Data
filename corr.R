corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all 
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0

	## Return a numeric vector of correlations
	## NOTE: Do not round the result!

	setwd(paste('C:/Users/Cauan/Documents/',directory, sep = ""))

	temp = list.files(pattern="*.csv")
	mydata = lapply(temp, read.csv, colClasses = c("NULL", NA, NA, "NULL")) # only 'sulfate' and 'nitrate' columns exist in 'mydata'

	cc <- complete('specdata')

	x <- split(mydata, cc$nobs >= threshold)
	x <- x['TRUE']
	
	mylist <- unlist(x, recursive = FALSE, use.names = FALSE)
	
	sapply(mylist, function(x) cor(x[[1]], x[[2]], use = "pairwise.complete.obs"))
	

}