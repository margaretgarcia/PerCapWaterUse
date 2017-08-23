# PerCapWaterUse
Model of per capita water use in the Las Vegas metropolitan area

This set of R files was developed to comparatively test a set of models to estimate 
per capita demand. The four demand models are calibrated and validated using data
from the Las Vegas Valley Water District which serves the City of Las Vegas and surrounding
unincorporated areas. The main script file is LVDemand_Script.R. This script produces 
figures of modeled and observed demand as well as calibration and validation period performance 
statistics. This script file is dependent on the following required packages, functions
and dataframes. Dataframes and function are included in this folder while packages can
be downloaded from the web.

Required packages include: hydroGOF, plyr, reshape2, ggplot2, gridExtra

Required functions include: dem1.R, dem2.R, dem3.R, dem4.R, calSep.R, valSep.R

Required dataframes include: refModeMonthly, externalVars, decisionVars
