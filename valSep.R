valSep=function(nc,D,startDate){
  # valSep = function(nc,D,startDate)
  # Purpose: extract every other year for alternate year validation
  # Inputs:
    # nc = length of time series
    # D = full result vector
    # startDate = start date of time series
  # Outputs:
    # res = validation vector
  
  res <- D[1:nc]       # Set up results vector
  res[1:nc] <- 0
  res[1:12] <- D[13:24]
  k <- 25
  j <- 13
  date <- (startDate+2)
  while (j < nc) {     # Even years for validation
    if ((as.integer(date) %% 2) == 0) {
      res[j:(j+11)] <- D[k:(k+11)]
      date <- date + 1
      k <- k + 12
      j <- j + 12
    } else {           # Odd years used for calibration
      date <- date + 1
      k <- k + 12
    }
  }
  
  return(res)
}