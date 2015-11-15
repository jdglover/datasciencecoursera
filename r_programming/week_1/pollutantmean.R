pollutantmean <- function(directory, pollutant, id=1:332) {
    # Computes the mean of the pollutant across all monitors ignoring
    # NA values.
    #
    # Args:
    #    directory: A character vector indicating the location of the csv 
    #               files.
    #    pollutant: A character vector indicating the name of the pollutant, 
    #               either "sulfate" or "nitrate".
    #    id: An integer vector indicating the monitor ID numbers to be used.
    # Returns:
    #    The mean of the pollutant across all monitors listed in the 'id' vector
    #    (ignoring NA values)
    wd <- paste(getwd(), directory, sep="/")
    data_files <- list.files(wd, pattern="*.csv")
    master_df <- data.frame(matrix(ncol = 4, nrow = 0))
    for (each_csv in data_files[id]) {
        temp_df <- read.csv(paste(wd, each_csv, sep="/"))
        #print(head(temp_df))
        master_df <- rbind(master_df, temp_df)
        #print(master_df)
        #print(colnames(master_df))
    }
    #master_file <- read.csv(paste(wd, data_files[id[1]], sep="/"))
    #if(length(id) > 1) {
    #    idx <- 2
    #    for (i in id[2:length(id)]) {
    #        next_file <- read.csv(paste(wd, data_files[id[idx]], sep="/"))
    #        master_file <- rbind(master_file, next_file)
    #        idx <- idx + 1
    #    }
    #}
    return(mean(master_df[[pollutant]], na.rm=TRUE))
}