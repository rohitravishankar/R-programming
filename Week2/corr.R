corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!

	if(grep("specdata", directory) == 1) {
        directory <- ("/")
    	}
	 #getting the previously computed complete table
   	 complete_table <- complete("specdata", 1:332)
    	 nobs <- complete_table$nobs

   	 # find the valid ids
       idNobs <- complete_table$id[nobs > threshold]

   	 # get the length of idNobs vector
   	 id_len <- length(idNobs)
	 #create vector to 
    	 corr_vector <- rep(0, id_len)

    	all_files <- as.character( list.files(paste(getwd(), directory, sep="")) )
    	file_paths <- paste(getwd(), directory, all_files, sep="")

	count <- 1
   	for(i in idNobs) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        corr_vector[count] <- cor(current_file$sulfate, current_file$nitrate, use="complete.obs")
        count <- count + 1
    	}
   	result <- corr_vector
	return(result)   


}