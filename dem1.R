dem1=function(Pr,D1,elas,TC,B0,B1){
  # dem1 = (Pr,D1,elas,TC,B0,B1)
  # Purpose: project per capita demand
  # Inputs:
  #   Pr = monthly marginal water rate time series
  #   D1 = mean monthly demand for year prior to simulation for initialization 
  #   elas = demand elasticity
  #   TC = mean monthly temperature time series in Celcius
  #   B0 = intercept from seasonality regression
  #   B1 = coeficient from seasonality regression
  # Outputs:
  #   D1 = monthly per capita demand, from model 1
  
  dDdt1 <- numeric(length = length(P))
  initalMean <- D1[1]
  D1[1] <- D1[1] + B1*TC[1]*initalMean + B0
  t <- 2

  while (t < length(P)) {
    
    # Demand function 1 - price dynamics only
    dDdt1[t] <- elas*D1[t]*(Pr[t]-Pr[t-1])/Pr[t]
    D1[t+1] <- dDdt1[t]*dt + D1[t]
    
    # Add in seasonality
    if (t>11) {
      temp <- sum(D1[(t-11):t])
    } else{
      temp <- initalMean*12
    }
    D1[t] <- D1[t] + B1*TC[t]*temp + B0

    t <- t + 1
  }
  temp <- sum(D1[(t-11):t])
  D1[t] <- D1[t] + B1*TC[t]*temp + B0

  D1 <- D1[1:length(P)]
  return(D1)
}