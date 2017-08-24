dem3=function(P,M,D3,Dmin,Dcode,alpha,TC,B0,B1){
  # dem3 = (P,M,D3,Dmin,Dcode,alpha,TC,B0,B1)
  # Purpose: project per capita demand
  # Inputs:
  #   P = monthly population time series
  #   M = monthly time series of % media articles on water issues
  #   D3 = mean monthly demand for year prior to simulation for initialization 
  #   Dmin = cultural and technological mean per capita
  #   Dcode = monthly average per capita demand for current code
  #   alpha = system responsiveness to water stress (0-1), calibrated parameter
  #   TC = mean monthly temperature time series in Celcius
  #   B0 = intercept from seasonality regression
  #   B1 = coeficient from seasonality regression
  # Outputs:
  #   D3 = monthly per capita demand, from model 3
  
  beta3 <- numeric(length = length(P))
  dDdt3 <- numeric(length = length(P))
  initalMean <- D3[1]
  Pmax <- P[1]
  D3[1] <- D3[1] + B1*TC[1]*initalMean + B0
  t <- 2
  while (t < length(P)) {
    
    # Demand function 3 - event drive plus tech/reg trend 
    deltaP <- P[t+1]- Pmax
    if (deltaP < 0) {deltaP <- 0}
    beta3[t] <- 1-(D3[t]*P[t] + Dcode[t]*(deltaP))/D3[t]/P[t+1]
    dDdt3[t] <- D3[t]*(1/dt*((D3[t]*P[t] + Dcode[t]*(deltaP))/D3[t]/P[t+1] - 1) + M[t]*alpha*(Dmin/D3[t]-1))
    D3[t+1] <- dDdt3[t]*dt + D3[t]
    if (P[t+1]>Pmax) {Pmax <- P[t+1]}
    
    # Add in seasonality
    if (t>11) {
      temp <- sum(D3[(t-11):t])
    } else{
      temp <- initalMean*12
    }
    D3[t] <- D3[t] + B1*TC[t]*temp + B0

    t <- t + 1
    
  }
  temp <- sum(D3[(t-11):t])
  D3[t] <- D3[t] + B1*TC[t]*temp + B0

  D3 <- D3[1:length(P)]
  return(D3)
}