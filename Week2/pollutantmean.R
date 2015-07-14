pollutantmean <- function(directory, pollutant = "sulfate", id = 1:10) {
    ## 'directory' is alsharacter vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    # set working directory
    if(grep("specdata", directory) == 1) {
        directory <- ("/")
    }
    # initialize a vector to hold the pollutant data
    mean_vector <- c()
    all_files <- as.character( list.files(paste(getwd(), directory, sep="")) )
    file_paths <- paste(getwd(), directory, all_files, sep="")
    for(i in id) {
		current_file <- read.csv(file_paths[i], header=T, sep=",")
		if(!exists("append1")){
			append1 <- current_file
		} else{
			append1 <- rbind(append1,current_file)
		}
			
    }
    result <- mean(append1[[pollutant]],na.rm=T)
    print(result)
}
