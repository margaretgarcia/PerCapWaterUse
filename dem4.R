dem4=function(P,M,Pr,D4,Dmin,Dcode,alpha,elas,TC,B0,B1){
  # dem4 = (P,M,Pr,D4,Dmin,Dcode,alpha,elas,TC,B0,B1)
  # Purpose: project per capita demand
  # Inputs:
  #   P = monthly population time series
  #   M = monthly time series of % media articles on water issues
  #   Pr = monthly marginal water rate time series
  #   D4 = mean monthly demand for year prior to simulation for initialization 
  #   Dmin = cultural and technological mean per capita demand
  #   Dcode = monthly average per capita demand for current code
  #   alpha = system responsiveness to water stress (0-1), calibrated parameter
  #   elas = demand elasticity
  #   TC = mean monthly temperature time series in Celcius
  #   B0 = intercept from seasonality regression
  #   B1 = coeficient from seasonality regression
  # Outputs:
  #   D4 = monthly per capita demand, from model 4
  
  beta4 <- numeric(length = length(P))
  dDdt4 <- numeric(length = length(P))
  initalMean <- D4[1]
  Pmax <- P[1]
  D4[1] <- D4[1] + B1*TC[1]*initalMean + B0
  t <- 2
  while (t < length(P)) {

    # Demand function 4 - price, event, tech/reg 
    deltaP <- P[t+1]- Pmax
    if (deltaP < 0) {deltaP <- 0}
    beta4[t] <- 1-(D4[t]*P[t] + Dcode[t]*(deltaP))/D4[t]/P[t+1]
    dDdt4[t] <- D4[t]*(elas*(Pr[t]-Pr[t-1])/Pr[t] + 1/dt*((D4[t]*P[t] + Dcode[t]*(deltaP))/D4[t]/P[t+1] - 1) + M[t]*alpha*(Dmin/D4[t]-1))
    D4[t+1] <- dDdt4[t]*dt + D4[t]
    if (P[t+1]>Pmax) {Pmax <- P[t+1]}
    
    # Add in seasonality
    if (t>11) {
      temp <- sum(D4[(t-11):t])
    } else{
      temp <- initalMean*12
    }
    D4[t] <- D4[t] + B1*TC[t]*temp + B0

    t <- t + 1
  }
  temp <- sum(D4[(t-11):t])
  D4[t] <- D4[t] + B1*TC[t]*temp + B0

  D4 <- D4[1:length(P)]
  return(D4)
}
