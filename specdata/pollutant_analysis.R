pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # Create file paths by formatting IDs and appending '.csv' to each
  fileNames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv")
  
  # Read all specified files into a list of data tables
  lst <- lapply(fileNames, fread)
  
  # Combine all data tables into one
  dt <- rbindlist(lst)
  
  # Check if the specified pollutant exists in the data table and calculate the mean
  if (pollutant %in% names(dt)) {
    return(dt[, lapply(.SD, mean, na.rm = TRUE), .SDcols = pollutant][[1]])
  } else {
    stop("Pollutant not found in data")
  }
}
