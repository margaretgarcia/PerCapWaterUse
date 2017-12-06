# server.R

library(ggplot2) 
library(plyr)
library(reshape2)
library(gridExtra)
library(hydroGOF)

# Load functions
# setwd("C:/Users/mgarc120/Dropbox (ASU)/Research/Modeling/DemandModel")
source("calSep.R")

# Load data
options(stringsAsFactors = FALSE)
load(file="refModeMonthly.R")
load(file="externalVars.R")
load(file="decisionVars.R")

shinyServer(
  function(input, output, clientData, session) {
    source("dem1.R")
    source("dem2.R")
    source("dem3.R")
    source("dem4.R")

    #Set simulation time period
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
    fnD <- matrix(ncol = length(P), nrow = n)
    ssq <- matrix(ncol = 1, nrow = n)
    nc <- 96 # calibration period length
    calRes <- matrix(ncol = nc, nrow = n)

    output$Plot <- renderPlot({
      if (input$Model == "Price Only") {
        DM <- dem1(Pr,D,elas,TC,B0,B1)
      } else if (input$Model == "Price & Building Codes") {
        DM <- dem2(P,Pr,D,Dcode,elas,TC,B0,B1)
      } else if (input$Model == "Building Codes & Stress Response") {
        k <- 1
        while (k<n+1) {
          fnD[k,] <- dem3(P,M,D,Dmin,Dcode,alpha[k],TC,B0,B1)
          k <- k + 1
        }
        k <- 1
        while (k<n+1) {
          calRes[k,] <- calSep(nc,fnD[k,],1997)
          k <- k + 1
        }
        Dcal <- calSep(nc,Dobs,1997)
        k <- 1
        while (k <= n) {
          difSquared <- (calRes[k,] -Dcal)^2
          ssq[k,] <- sum(difSquared)
          k <- k + 1
        }
        ssq <- unlist(ssq)
        id <- which.min(ssq)
        alphaCal <- alpha[id]
        ssq <- ssq[id]
        DM <- dem3(P,M,D,Dmin,Dcode,alphaCal,TC,B0,B1)
      } else if (input$Model == "Price, Codes & Stress Response") {
        k <- 1
        while (k<n+1) {
          fnD[k,] <- dem4(P,M,Pr,D,Dmin,Dcode,alpha[k],elas,TC,B0,B1)
          k <- k + 1
        }
        k <- 1
        while (k<n+1) {
          calRes[k,] <- calSep(nc,fnD[k,],1997)
          k <- k + 1
        }
        Dcal <- calSep(nc,Dobs,1997)
        k <- 1
        while (k <= n) {
          difSquared <- (calRes[k,] -Dcal)^2
          ssq[k,] <- sum(difSquared)
          k <- k + 1
        }
        ssq <- unlist(ssq)
        id <- which.min(ssq)
        alphaCal <- alpha[id]
        ssq <- ssq[id]
        DM <- dem4(P,M,Pr,D,Dmin,Dcode,alphaCal,elas,TC,B0,B1)
      }
      
      # US customary units to SI
      Dobs <- Dobs*3.79 # liters
      DM <- DM*3.79 # liters
      
      dates <- dates <- seq(from = 1997,to = (2012+11/12), by = 1/12)
      dfTest <- data.frame(dates,Dobs,DM)
      colnames(dfTest) <- c("Year","Observed","Modeled")
      dfTest <- melt(dfTest, id=c("Year")) 

      # Plot model results and compare to data
      pd <- ggplot(data=dfTest, aes(x=Year, y = value, colour= variable)) +  geom_line(size=1.1) + 
        geom_point(size = 2) +
        scale_colour_brewer(palette = "Set1") +
        ggtitle(paste(input$Model)) +
        theme(legend.position = "bottom") +
        ylab("Per Capita Usage (L/day)") + theme(text = element_text(size = 12))
      

      pd
      
    })
    
  })