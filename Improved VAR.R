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
# data read-in and adjustments

date.start <- '1993-01-01'

# monthly data
cpi <- ts_cansim("v41690914", start = date.start) # cpi, monthly, SA, 2002=100
gdp.m <- ts_cansim("v65201210", start = date.start) # gdp, monthly, SA, 2012=100
unemp.rt <- ts_cansim("v2062815", start = date.start) # monthly, unemployment rate, SA
wage.rt <- ts_cansim("v2132579", start = date.start) # average hourly wage, monthly, not SA, 2022 dollars
bank.rt <- ts_cansim("v122530", start = date.start) # bank rate, monthly
trgt.rt <- bank.rate - 0.25

# quarterly data
gdp.q <- ts_cansim("v62305752", start = date.start) # gdp, quarterly, SA, 2012=100
hh.cons <- ts_cansim("v62305724", start = date.start) # households' final consumption expenditure, quarterly, SA, 2012=100
nphh.cons <- ts_cansim("v62305723", start = date.start) # non-profits serving households' consumption expenditure, quarterly, SA, 2012=100
govt.cons <- ts_cansim("v62305731", start = date.start) # governments final consumption expenditure, quarterly, SA, 2012=100
bus.cap <- ts_cansim("v62305733", start = date.start) # business gross fixed capital formation, quarterly, SA, 2012=100
govt.cap <- ts_cansim("v62305740", start = date.start) # government gross fixed capital formation, quarterly, SA, 2012=100
nphh.cap <- ts_cansim("v62305739", start = date.start) # non-profits serving households' fixed capital formation, quarterly, SA, 2012=100
bus.invrs <- ts_cansim("v62305741", start = date.start) # investment in inventories, quarterly, SA, 2012=100
xprts <- ts_cansim("v62305745", start = date.start) # exports, quarterly, SA, 2012=100
mprts <- ts_cansim("v62305748", start = date.start) # imports, quarterly, SA, 2012=100
capu.rt <- ts_cansim("v4331081", start = date.start) # Total Industrial Capacity Utilization rate, quarterly 


# create gdp component variables
con <- hh.cons + nphh.cons + nphh.cons
gov <- govt.cons + govt.cap 
inv <- bus.cap + bus.invrs + nphh.cap
nex <- xprts - mprts

##
# checking for stationarity
main.data <- cbind(cpi, gdp.m, unemp.rt, bank.rt)

plot(capu.rt)

summary(ur.df(capu.rt))

plot(cbind(capu.rt, diff(capu.rt), diff(capu.rt,4)))

ndiffs(diff(trgt.rt, 12))

plot(cbind(capu.rt, diff(capu.rt), diff(capu.rt,4)))

summary(ur.df(capu.rt))
summary(ur.df(diff(capu.rt)))
summary(ur.df(diff(capu.rt, 4)))

plot(cbind(con, gov, inv, nex))

##
# making stationary
CPI <- diff(log(cpi), 12)
GDP.M <- diff(log(gdp.m), 12)
UNEMP <- diff(unemp.rt, 12)
TRGT <- diff(trgt.rt, 12)

GDP.Q <- diff(log(gdp.q), 4)
CON <- diff(log(con), 4)
GOV <- diff(log(gov), 4)
INV <- diff(log(inv), 4)
NEX <- diff(log(nex+100000), 4) # add constant (100,000)

CAPU <- diff(capu.rt)































