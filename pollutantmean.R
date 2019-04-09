pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating 
	## the location of the CSV files

	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!

	setwd(paste('C:/Users/Cauan/Documents/',directory, sep = ""))

	temp = list.files(pattern="*.csv")
	specdata = lapply(temp, read.csv)

	myfiles = specdata[c(id)]

	mylist = lapply(myfiles, '[[', pollutant)
	
	mean(unlist(mylist), na.rm = T)

}