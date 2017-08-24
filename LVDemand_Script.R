# Purpose: test alternate formulation of the demand function
# Author: M Garcia
# Date: August 1, 2016
# Revised: June 16, 2017

# Load libraries
library(ggplot2) 
library(plyr)
library(reshape2)
library(gridExtra)
library(hydroGOF)

# Load functions
# setwd("C:/Users/mgarc120/Dropbox (ASU)/Research/Modeling/DemandModel")
source("dem1.R")
source("dem2.R")
source("dem3.R")
source("dem4.R")
source("calSep.R")
source("valSep.R")

# Load data
options(stringsAsFactors = FALSE)
load(file="refModeMonthly.R")
load(file="externalVars.R")
load(file="decisionVars.R")

# Organize data for function testing
TC <- externalVars[,"tempC"]
Pr <- decisionVars[,"effPrice"]
Dcode <- decisionVars[,"dCode"]
P <- externalVars[,"population"]
M <- refModeMonthly[,"mediaPeaks"]
Dobs <- refModeMonthly[,"perCap"]
TP <- length(Pr)
D <- numeric(length = TP)     # Per capita demand vector
n <- length(P)
dates <- seq(from = 1,to =n, by = 1)

# Initial conditions
t <- 1
D[t] <- mean(Dobs[t:(t+11)])
t <- 2
D[t] <-  mean(Dobs[t:(t+11)])

# Speciy parameter values
Dmin <- 125 # Gal per capita per day
alpha <- seq(from=0.01, to = 10, by = 0.05)
elas <- -0.24
dt <- 1
B0 <- -1.576e+02  # Seasonal model regression intercept
B1 <- 2.485e-03   # Seasonal model regression coeficient 

n <- length(alpha)
fnD4 <- matrix(ncol = length(P), nrow = n)
ssq4 <- matrix(ncol = 1, nrow = n)
fnD3 <- matrix(ncol = length(P), nrow = n)
ssq3 <- matrix(ncol = 1, nrow = n)
nc <- 96 # calibration period length
calRes3 <- matrix(ncol = nc, nrow = n)
calRes4 <- matrix(ncol = nc, nrow = n)

# Compute modeled demand using each of the four functions
k <- 1
while (k<n+1) {
  fnD4[k,] <- dem4(P,M,Pr,D,Dmin,Dcode,alpha[k],elas,TC,B0,B1)
  fnD3[k,] <- dem3(P,M,D,Dmin,Dcode,alpha[k],TC,B0,B1)
  k <- k + 1
}

# Extract results for calibration years: 1991, 1993 to 2005
k <- 1
while (k<n+1) {
  calRes3[k,] <- calSep(nc,fnD3[k,],1997)
  calRes4[k,] <- calSep(nc,fnD4[k,],1997)
    k <- k + 1
}
Dcal <- calSep(nc,Dobs,1997)

# Compute ssq, need to skip alternate in computations
k <- 1
while (k <= n) {
  difSquared4 <- (calRes4[k,] -Dcal)^2
  ssq4[k,] <- sum(difSquared4)
  difSquared3 <- (calRes3[k,] -Dcal)^2
  ssq3[k,] <- sum(difSquared3)
  k <- k + 1
}

# Convert to vector, find min
ssq3 <- unlist(ssq3)
i3 <- which.min(ssq3)
alphaCal3 <- alpha[i3]
ssq3 <- ssq3[i3]
ssq4 <- unlist(ssq4)
i4 <- which.min(ssq4)
alphaCal4 <- alpha[i4]
ssq4 <- ssq4[i4]

# Get all results for performance stats
D1 <- dem1(Pr,D,elas,TC,B0,B1)
D2 <- dem2(P,Pr,D,Dcode,elas,TC,B0,B1)
D3 <- dem3(P,M,D,Dmin,Dcode,alphaCal3,TC,B0,B1)
D4 <- dem4(P,M,Pr,D,Dmin,Dcode,alphaCal4,elas,TC,B0,B1)

# Extract results for calibration years: 1997, 1999 to 2011
res1 <- calSep(nc,D1,1997)
res2 <- calSep(nc,D2,1997)
res3 <- calSep(nc,D3,1997)
res4 <- calSep(nc,D4,1997)

# Compute sum of the squares, correlation coef, % bias and nash efficiency
calSSQ <- numeric(length=4)
calCor <- numeric(length=4)
calPB <- numeric(length=4)
calNSE <- numeric(length=4)
calRMSE <- numeric(length=4)
difSquared1 <- (res1 -Dcal)^2
calSSQ[1] <- sum(difSquared1)
difSquared2 <- (res2 -Dcal)^2
calSSQ[2] <- sum(difSquared2)
difSquared3 <- (res3 -Dcal)^2
calSSQ[3] <- sum(difSquared3)
difSquared4 <- (res4 -Dcal)^2
calSSQ[4] <- sum(difSquared4)
calCor[1] <- cor(res1,Dcal)
calCor[2] <- cor(res2,Dcal)
calCor[3] <- cor(res3,Dcal)
calCor[4] <- cor(res4,Dcal)
calPB[1] <- pbias(res1,Dcal)
calPB[2] <- pbias(res2,Dcal)
calPB[3] <- pbias(res3,Dcal)
calPB[4] <- pbias(res4,Dcal)
calNSE[1] <- rNSE(res1,Dcal)
calNSE[2] <- rNSE(res2,Dcal)
calNSE[3] <- rNSE(res3,Dcal)
calNSE[4] <- rNSE(res4,Dcal)
calRMSE[1] <- rmse(res1,Dcal)
calRMSE[2] <- rmse(res2,Dcal)
calRMSE[3] <- rmse(res3,Dcal)
calRMSE[4] <- rmse(res4,Dcal)
calAltStats <- data.frame(calSSQ,calCor,calPB,calNSE,calRMSE) # Combine stats in a table
save(calAltStats, file="calAltStats.R")

# Extract results for validation years: 1998, 2000 to 2012
val1 <- valSep(nc,D1,1997)
val2 <- valSep(nc,D2,1997)
val3 <- valSep(nc,D3,1997)
val4 <- valSep(nc,D4,1997)
Dval <- valSep(nc,Dobs,1997)

# Compute sum of the squares, correlation coef, % bias and nash efficiency
valSSQ <- numeric(length=4)
valCor <- numeric(length=4)
valPB <- numeric(length=4)
valNSE <- numeric(length=4)
valRMSE <- numeric(length=4)
difSquared1 <- (val1 -Dval)^2
valSSQ[1] <- sum(difSquared1)
difSquared2 <- (val2 -Dval)^2
valSSQ[2] <- sum(difSquared2)
difSquared3 <- (val3 -Dval)^2
valSSQ[3] <- sum(difSquared3)
difSquared4 <- (val4 -Dval)^2
valSSQ[4] <- sum(difSquared4)
valCor[1] <- cor(val1,Dval)
valCor[2] <- cor(val2,Dval)
valCor[3] <- cor(val3,Dval)
valCor[4] <- cor(val4,Dval)
valPB[1] <- pbias(val1,Dval)
valPB[2] <- pbias(val2,Dval)
valPB[3] <- pbias(val3,Dval)
valPB[4] <- pbias(val4,Dval)
valNSE[1] <- rNSE(val1,Dval)
valNSE[2] <- rNSE(val2,Dval)
valNSE[3] <- rNSE(val3,Dval)
valNSE[4] <- rNSE(val4,Dval)
valRMSE[1] <- rmse(val1,Dval)
valRMSE[2] <- rmse(val2,Dval)
valRMSE[3] <- rmse(val3,Dval)
valRMSE[4] <- rmse(val4,Dval)
valAltStats <- data.frame(valSSQ,valCor,valPB,valNSE,valRMSE) # Combine stats in a table
save(valAltStats, file="valAltStats.R")


# US customary units to SI
Dobs <- Dobs*3.79 # liters
D1 <- D1*3.79 # liters
D2 <- D2*3.79 # liters
D3 <- D3*3.79 # liters
D4 <- D4*3.79 # liters

# Combine for plotting
dates <- dates <- seq(from = 1997,to = (2012+11/12), by = 1/12)
dfTest1 <- data.frame(dates,Dobs,D1)
dfTest2 <- data.frame(dates,Dobs,D2)
dfTest3 <- data.frame(dates,Dobs,D3)
dfTest4 <- data.frame(dates,Dobs,D4)
colnames(dfTest1) <- c("Year","Dobs","D1")
dfTest1 <- melt(dfTest1, id=c("Year")) 
colnames(dfTest2) <- c("Year","Dobs","D2")
dfTest2 <- melt(dfTest2, id=c("Year")) 
colnames(dfTest3) <- c("Year","Dobs","D3")
dfTest3 <- melt(dfTest3, id=c("Year")) 
colnames(dfTest4) <- c("Year","Dobs","D4")
dfTest4 <- melt(dfTest4, id=c("Year")) 

# Plot model results and compare to data
pd1 <- ggplot(data=dfTest1, aes(x=Year, y = value, colour= variable)) +  geom_line(size=1.1) + 
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle(paste("Demand = f(Price)")) +
  theme(legend.position = "bottom") +
  ylab("Per Capita Usage (L/day)") + theme(text = element_text(size = 12))

pd2 <- ggplot(data=dfTest2, aes(x=Year, y = value, colour= variable)) +  geom_line(size=1.1) + 
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle(paste("Demand = f(Price, Reg trend)")) +
  theme(legend.position = "bottom") +
  ylab("Per Capita Usage (L/day)") + theme(text = element_text(size = 12))

pd3 <- ggplot(data=dfTest3, aes(x=Year, y = value, colour= variable)) +  geom_line(size=1.1) + 
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle(paste("Demand = f(Reg trend, Stress Response)")) +
  theme(legend.position = "bottom") +
  ylab("Per Capita Usage (L/day)") + theme(text = element_text(size = 12))

pd4 <- ggplot(data=dfTest4, aes(x=Year, y = value, colour= variable)) +  geom_line(size=1.1) + 
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Set1") +
  ggtitle(paste("Demand = f(Price, Reg trend, Stress Response)")) +
  theme(legend.position = "bottom") +
  ylab("Per Capita Usage (L/day)") + theme(text = element_text(size = 12))

grid.arrange(pd1, pd2, pd3, pd4, ncol=2)
