complete <- function(directory, id = 1:332) {
    # Filters for complete cases from monitors
    #
    # Args:
    #    directory: A character vector of length 1 indicating the location 
    #               of the CSV files.
    #    id: An integer vector indicating the monitor ID numbers to be used.
    # Returns:
    #    A data frame where 'id' is the monitor ID number and 'nobs' is the
    #    number of complete cases.
    wd <- paste(getwd(), directory, sep="/")
    data_files <- list.files(wd, pattern="*.csv")
    columns <- c("sulfate", "nitrate", "ID")
    results_df <- data.frame(matrix(ncol = 2, nrow = 0))
    for (each in id) {
        temp_df <- read.csv(paste(wd, data_files[each], sep="/"))
        complete_rows <- complete.cases(temp_df[columns])
        temp_df <- temp_df[complete_rows, columns]
        values <- (c(temp_df[1, columns[3]], nrow(temp_df)))
        results_df <- rbind(results_df, values)
    }
    colnames(results_df) <- c("id", "nobs")
    return(results_df)
}