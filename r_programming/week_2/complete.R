complete <- function(directory, id = 1:332) {
    # Counts complete cases from air quality monitors
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
    for (file_idx in id) {
        temp_df <- read.csv(paste(wd, data_files[file_idx], sep="/"))
        complete_rows <- temp_df[complete.cases(temp_df[columns]), columns]
        temp_df_values <- (c(temp_df[1, columns[3]], nrow(complete_rows)))
        results_df <- rbind(results_df, temp_df_values)
    }
    colnames(results_df) <- c("id", "nobs")
    return(results_df)
}