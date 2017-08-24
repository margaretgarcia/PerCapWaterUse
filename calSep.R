calSep=function(nc,D,startDate){
  # calSep = function(nc,D,startDate)
  # Purpose: extract every other year for alternate year calibration
  # Inputs:
    # nc = length of time series
    # D = full result vector
    # startDate = start date of time series
  # Outputs:
    # res = calibration vector
  
  res <- D[1:nc]       # Set up results vector
  res[1:nc] <- 0
  res[1:12] <- D[1:12]
  k <- 13
  j <- 13
  date <- (startDate+1)
  while (j < nc) {     # Even years saved for validation
    if ((as.integer(date) %% 2) == 0) {
      date <- date + 1
      k <- k + 12
    } else {           # Odd years for calibration
      res[j:(j+11)] <- D[k:(k+11)]
      date <- date + 1
      k <- k + 12
      j <- j + 12
    }
  }
  
  return(res)
}