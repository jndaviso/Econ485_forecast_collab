#####
## ECON 485 - Group 1 - Forecasting Code v.2
# Now with improved VAR models!

## Initialize
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# lab wd
setwd("/Users/labuser/Desktop/temp_wd")

# desktop gdrive wd
setwd("C:/Users/James/My Drive/a.Classes/Fall 2022/ECON 485/Forecasting/temp_wd")

# mac gdrive wd



# Packages
library(lubridate)    # Easy date conversions
library(cansim)       # Get data from StatsCan
#library(OECD)         # Get data from OECD
#library(WDI)          # Get data from World Bank
library(fredr)        # Get data from FRED
library(forecast)     # Time Series Forecasting
library(vars)         # Vector Autoregression
library(plotrix)      # Draw a circle
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(writexl)

# Functions
source("functions/ts_cansim.R")
source("functions/ts_fred.R")
source("functions/ts_oecd.R")

#####
# data read-in

date.start <- '1993-01-01'

# monthly data
cpi <- ts_cansim("v41690914", start = date.start) # cpi, monthly, SA, 2002=100
unemp.rate <- ts_cansim("v2062815", start = date.start) # monthly, unemployment rate, SA
wage.rate <- ts_cansim("v2132579", start = date.start) # average hourly wage, monthly, not SA, 2022 dollars
bank.rate <- ts_cansim("v122530", start = date.start) # bank rate, monthly


# quarterly data
gdp <- ts_cansim("v62305752", start = date.start) # gdp, quarterly, SA, 2012=100
capu.rate <- ts_cansim("v4331081", start = date.start) # Total Industrial Capacity Utilization rate, quarterly 
hh.cons.exp <- ts_cansim("v62305724", start = date.start) # households' final consumption expenditure, quarterly, SA, 2012=100
nphh.cons.exp <- ts_cansim("v62305723", start = date.start) # non-profits serving households' consumption expenditure, quarterly, SA, 2012=100
govt.cons.exp <- ts_cansim("v62305731", start = date.start) # governments final consumption expenditure, quarterly, SA, 2012=100
bus.fix.cap <- ts_cansim("v62305733", start = date.start) # business gross fixed capital formation, quarterly, SA, 2012=100
govt.fix.cap <- ts_cansim("v62305740", start = date.start) # government gross fixed capital formation, quarterly, SA, 2012=100
nphh.fix.cap <- ts_cansim("v62305739", start = date.start) # non-profits serving households' fixed capital formation, quarterly, SA, 2012=100
bus.invrs <- ts_cansim("v62305741", start = date.start) # investment in inventories, quarterly, SA, 2012=100
xprts <- ts_cansim("v62305745", start = date.start) # exports, quarterly, SA, 2012=100
mprts <- ts_cansim("v62305748", start = date.start) # imports, quarterly, SA, 2012=100
