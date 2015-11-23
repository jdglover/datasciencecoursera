corr <- function(directory, threshold = 0) {
    # Calculates the correlation between two air pollutants if the set of
    # complete cases is greater than the threshold.
    #
    # Args:
    #    directory: A character vector of length 1 indicating the location 
    #               of the CSV files.
    #    threshold: A numeric vector of length 1 indicating the number of 
    #               completely observed observations (on all variables) 
    #               required to compute the correlation between nitrate and 
    #               sulfate; the default is 0.
    # Returns:
    #    A numeric vector of correlations.
    wd <- paste(getwd(), directory, sep="/")
    data_files <- list.files(wd, pattern="*.csv")
    corr_vector <- vector(mode="numeric", length=0)
    for (each_csv in data_files) {
        temp_df <- read.csv(paste(wd, each_csv, sep="/"))
        complete_rows <- temp_df[complete.cases(temp_df),]
        if (nrow(complete_rows) > threshold) {
            pollutant_corr <- cor(complete_rows[,"sulfate"], complete_rows[,"nitrate"])
            corr_vector <- c(corr_vector, pollutant_corr)
        }
    }
    return(corr_vector)
}