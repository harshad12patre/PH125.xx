complete <- function(directory,  id = 1:332) {
  
  # Format number with fixed width and then append .csv to number
  fileNames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
  
  # Reading in all files and making a large data.table
  lst <- lapply(fileNames, data.table::fread)
  dt <- rbindlist(lst)
  
  return(dt[complete.cases(dt), .(nobs = .N), by = ID])
  
}

RNGversion("3.5.1")  
set.seed(42, sample.kind = "Rounding")
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
