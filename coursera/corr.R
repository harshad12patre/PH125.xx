corr <- function(directory, threshold = 0) {
  
  # Reading in all files and making a large data.table
  lst <- lapply(file.path(directory, list.files(path = directory, pattern="*.csv")), data.table::fread)
  dt <- rbindlist(lst)
  
  # Only keep completely observed cases
  dt <- dt[complete.cases(dt),]
  
  # Apply threshold
  dt <- dt[, .(nobs = .N, corr = cor(x = sulfate, y = nitrate)), by = ID][nobs > threshold]
  return(dt[, corr])
}

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))