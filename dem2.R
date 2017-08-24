dem2=function(P,Pr,D2,Dcode,elas,TC,B0,B1){
  # dem2 = (P,Pr,D2,Dcode,elas,TC,B0,B1)
  # Purpose: project per capita demand
  # Inputs:
  #   P = monthly population time series
  #   Pr = monthly marginal water rate time series
  #   D2 = mean monthly demand for year prior to simulation for initialization 
  #   Dcode = monthly average per capita demand for current code
  #   elas = demand elasticity
  #   TC = mean monthly temperature time series in Celcius
  #   B0 = intercept from seasonality regression
  #   B1 = coeficient from seasonality regression
  # Outputs:
  #   D2 = monthly per capita demand, from model 2
  
  dDdt2 <- numeric(length = length(P))
  initalMean <- D2[1]
  Pmax <- P[1]
  D2[1] <- D2[1] + B1*TC[1]*initalMean + B0
  t <- 2
  while (t < length(P)) {
    
    # Demand function 2 - price plus tech/reg trend
    deltaP <- P[t+1]- Pmax
    if (deltaP < 0) {deltaP <- 0}
    dDdt2[t] <- D2[t]*(elas*(Pr[t]-Pr[t-1])/Pr[t]+ 1/dt*((D2[t]*P[t] + Dcode[t]*(deltaP))/D2[t]/P[t+1] - 1))
    D2[t+1] <- dDdt2[t]*dt + D2[t]
    if (P[t+1]>Pmax) {Pmax <- P[t+1]}
    
    # Add in seasonality
    if (t>11) {
      temp <- sum(D2[(t-11):t])
    } else{
      temp <- initalMean*12
    }
    D2[t] <- D2[t] + B1*TC[t]*temp + B0

    t <- t + 1
  }
  temp <- sum(D2[(t-11):t])
  D2[t] <- D2[t] + B1*TC[t]*temp + B0

  D2 <- D2[1:length(P)]
  return(D2)
}