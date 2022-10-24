#####
## ECON 485 - Group 1 - Forecasting Code v.2
# Now with improved VAR models!

## Initialize
rm(list = ls())
dev.off(dev.list()["RStudioGD"])

# lab wd
# setwd("/Users/labuser/Desktop/temp_wd")

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
trgt.rt <- bank.rt - 0.25

us.gdp <- ts(data.frame(read.csv(file = "data/realgdpindex_ihsmarkit.csv"))$value, start = c(1992, 01), frequency = 12) # read in csv for us monthly gdp,, no SA
fredr_set_key('7eb7c5788c21aacbba171d29b877f086')
wti <- ts_fred('MCOILWTICO', start = date.start) # oil price, monthly, no SA

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

###
# checking for stationarity
main.data <- cbind(cpi, gdp.m, unemp.rt, bank.rt)

plot(wti)

summary(ur.df(wti))

plot(cbind(wti, diff(wti), diff(wti,12), diff(log(wti),12)))

ndiffs(diff(trgt.rt, 12))

plot(cbind(capu.rt, diff(capu.rt), diff(capu.rt,4)))

summary(ur.df(capu.rt))
summary(ur.df(diff(capu.rt)))
summary(ur.df(diff(capu.rt, 4)))

plot(cbind(con, gov, inv, nex))

###
# making stationary
CPI <- diff(log(cpi), 12)
GDP.M <- diff(log(gdp.m), 12)
#UNEMP <- diff(unemp.rt, 12)
UNEMP <- unemp.rt
# TRGT <- diff(trgt.rt, 12)
TRGT <- trgt.rt

GDP.Q <- diff(log(gdp.q), 4)
CON <- diff(log(con), 4)
GOV <- diff(log(gov), 4)
INV <- diff(log(inv), 4)
NEX <- diff(log(nex+100000), 4) # add constant (100,000)

CAPU <- diff(capu.rt)

X.USGDP <- diff(log(us.gdp), 12)

# if wti stationary
# WTI <- diff(log(wti), 12)
X.WTI <- wti


#####
# Filling all Missing Values for CPI, GDP.M, UNEMP, TRGT, X.USGDP, X.WTI

# construct the restriction matrix
data.mainx <- cbind(CPI, GDP.M, UNEMP, TRGT)

date.est.start <- c(1998, 1)

exomat <- cbind(X.WTI,X.USGDP)
tail(exomat)
head(exomat)

# combine in window
data.exo <- window(cbind(data.mainx, exomat), 
                   start = date.est.start)

colnames(data.exo) <- c('CPI', 'GDP.M', 'UNEMP', 
                        'TRGT', 'X.WTI', 'X.USGDP')
head(data.exo)
tail(data.exo)

# create estimation window from complete data
date.est.end <- c(2022, 7)
data.exo.july <- window(data.exo, start = date.est.start, end = date.est.end)

tail(data.exo.july)

# estimate the model without restrictions
VARselect(data.exo.july, 6)
# Selects a lag length of 3
lags <- 3
mod.est <- VAR(data.exo.july, p = lags)
coef(mod.est)

# get the coefficient matrix to put in the restrictions
mat.coef <- sapply(coef(mod.est), function(x) x[,'Estimate'])

# Need the transpose of this matrix to get the dimensions right for our restrictions matrix
mat.coef.res <- t(mat.coef)
mat.coef.res[,] <- 1

# Impose the restrictions (This is going to change with different lag length!!!)
mat.coef.res
mat.coef.res[c('X.WTI','X.USGDP'), 1:4] <- 0
mat.coef.res[c('X.WTI','X.USGDP'), 7:10] <- 0
mat.coef.res[c('X.WTI','X.USGDP'), 13:16] <- 0
mat.coef.res

# Re-estimate the model with the restrictions
mod.est.restrict <- restrict(mod.est, method = "man", resmat = mat.coef.res)

coef(mod.est.restrict)

# Granger Causality Testing
causality(mod.est.restrict, cause = 'X.WTI')
causality(mod.est.restrict, cause = 'X.USGDP')
# both pass

# Stability
roots(mod.est.restrict)

###
# Iterative Forecasting to fill in missing values

tail(data.exo)

# August missing GDP
# forecast august value
mod.restrict.fc <- forecast(mod.est.restrict, h = 1)
plot(mod.restrict.fc, include = 36)
# expand the data by one month
data.exo.aug <- window(data.exo, start = date.est.start, end = c(2022,8))

tail(data.exo.aug)

# input the forecast value to the dataset
n.aug <- nrow(data.exo.aug)
data.exo.aug[n.aug, 'GDP.M'] <- mod.restrict.fc$forecast$GDP.M$mean[1]
data.exo[n.aug, 'GDP.M'] <- mod.restrict.fc$forecast$GDP.M$mean[1]
tail(data.exo)

# Estimate a new model with the updated dataset and repeat for missing September gdp
mod.est <- VAR(data.exo.aug, p = lags)
mod.aug <- restrict(mod.est, method = "man", resmat = mat.coef.res)
mod.restrict.fc <- forecast(mod.aug, h = 1)

# expand the data by one month
data.exo.sept <- window(data.exo, start = date.est.start, end = c(2022,9))

# input the forecast value to the dataset
n.sept <- nrow(data.exo.sept)
data.exo.sept[n.sept, 'GDP.M'] <- mod.restrict.fc$forecast$GDP.M$mean[1]
data.exo.sept[n.sept, 'X.USGDP'] <- mod.restrict.fc$forecast$X.USGDP$mean[1]
data.exo[n.sept, 'GDP.M'] <- mod.restrict.fc$forecast$GDP.M$mean[1]
data.exo[n.sept, 'X.USGDP'] <- mod.restrict.fc$forecast$GDP.M$mean[1]

# the complete dataset
tail(data.exo)

#####
# VAR Model with TRGT ENDOGENOUS
# create data time window
data.main.raw <- cbind(CPI, GDP.M, UNEMP, TRGT)
head(data.main.raw)
tail(data.main.raw)

date.est.start <- c(1998, 1)
date.est.end <- c(2022, 7)
data.main <- window(data.main.raw, start = date.est.start, end = date.est.end)
tail(data.main)
head(data.main)

# estimating VAR model 
VARselect(data.main, lag.max = 6, type = "const") # why not 11?

n.lag <- 3

# Observe coefficients and roots
mod.var <- VAR(data.main,  p = n.lag, type = c("const"))
coef(mod.var)
roots(mod.var)

# check Granger Causality
causality(mod.var, cause = 'CPI')
causality(mod.var, cause = 'GDP.M')
causality(mod.var, cause = 'UNEMP')
causality(mod.var, cause = 'TRGT')
# gdp p = 0.0795

# forecast
fc.var <- forecast(mod.var, h = 29)
plot(fc.var, include = 90)

# GDP growth forecast
GDP.M.FC <- ts(c(GDP.M,fc.var$forecast$GDP.M$mean), start = start(GDP.M), frequency = frequency(GDP.M))
plot(GDP.M.FC)
lines(GDP.M, col = 'blue')

# CPI inflation forecast
CPI.FC <- ts(c(CPI,fc.var$forecast$GDP.M$mean), start = start(CPI), frequency = frequency(CPI))
plot(CPI.FC)
lines(CPI, col = 'blue')

# Unemployment rate forecast
UNEMP.FC <- ts(c(UNEMP,fc.var$forecast$UNEMP$mean), start = start(UNEMP), frequency = frequency(UNEMP))
plot(UNEMP.FC)
lines(UNEMP, col = 'blue')

# Target rate forecast
TRGT.FC <- ts(c(TRGT,fc.var$forecast$TRGT$mean), start = start(TRGT), frequency = frequency(TRGT))
plot(TRGT.FC)
lines(TRGT, col = 'blue')

#####
# VAR Model with TRGT and USGDP EXOGENOUS
# problem: coefficients of X.TRGT on GDP, CPI, UNEMP are extremely low,
# model appears to under predict the effect of TRGT when its exogenous
# why?

# create subset of main data set with GDP, CPI, UNEMP as endogenous
data.endo <- data.exo[ , 1:4]

tail(data.exo)

# exogenous matrix
exomat <- cbind(data.exo[ , 5], data.exo[ , 6])
colnames(exomat) <- c('X.WTI','X.USGDP')

# estimating VAR model 
VARselect(data.endo, lag.max = 6, type = "const", exogen = exomat)

n.lag <- 3
mod.var <- VAR(data.endo,  p = n.lag, type = c("const"), exogen = exomat)
coef(mod.var)
roots(mod.var)

# Specify path characteristics
h <- 29
X.WTI.path <- rep(NA, h)
x <- 9 # how many periods to reach terminal value
y <- 89 # terminal value

sequence <- seq(from = X.WTI[length(X.WTI)],
                to = y, 
                length.out = x)
for(i in 1:h){
  if(i <= x){
    X.WTI.path[i] <- sequence[i]
  } else{
    X.WTI.path[i] <- sequence[x]
  }
}

X.WTI.path

# USGDP path selection
h <- 29
X.USGDP.path <- rep(NA, h)
x <- 9 # how many periods to reach terminal value
# terminal value
# y <- mean(X.USGDP) 
y <- 0

sequence <- seq(from = X.USGDP[length(X.USGDP)],
                to = y,
                length.out = x)
for(i in 1:h){
  if(i <= x){
    X.USGDP.path[i] <- sequence[i]
  } else{
    X.USGDP.path[i] <- sequence[x]
  }
}

X.USGDP.path

# path matrix
exomat.path <- cbind(TRGT.path, X.USGDP.path)
colnames(exomat.path) <- c('TRGT','X.USGDP')

# forecast
fc.var <- forecast(mod.var, h = h, dumvar = exomat.path)
plot(fc.var, include = 36)

fc.var$model$varresult

#####
# VAR Model with USGDP and WTI EXOGENOUS

data.endo <- data.exo[ , 1:3]

# estimating VAR model 
VARselect(data.endo, lag.max = 6, type = "const") 

n.lag <- 3

# Observe coefficients and roots
mod.var <- VAR(data.endo,  p = n.lag, type = c("const"))
coef(mod.var)
roots(mod.var)

# check Granger Causality
causality(mod.var, cause = 'CPI') # fail
causality(mod.var, cause = 'GDP.M') # pass
causality(mod.var, cause = 'UNEMP') # pass

# exogenous matrix
exomat <- cbind(data.exo[ , 4], data.exo[ , 6])
colnames(exomat) <- c('TRGT','X.USGDP')

# estimate
VARselect(data.endo, lag.max = 6, type = "const", exogen = exomat)

n.lag <- 3
mod.var <- VAR(data.endo,  p = n.lag, type = c("const"), exogen = exomat)
coef(mod.var)
roots(mod.var)

# Specify path characteristics
h <- 29
TRGT.path <- rep(NA, h)
x <- 1 # how many periods to reach terminal value
y <- 5 # terminal value

sequence <- seq(from = TRGT[length(TRGT)],
                to = y, 
                length.out = x)
for(i in 1:h){
  if(i <= x){
    TRGT.path[i] <- sequence[i]
  } else{
    TRGT.path[i] <- sequence[x]
  }
}

TRGT.path

# USGDP path selection
h <- 29
X.USGDP.path <- rep(NA, h)
x <- 9 # how many periods to reach terminal value
# terminal value
# y <- mean(X.USGDP) 
y <- 0

sequence <- seq(from = X.USGDP[length(X.USGDP)],
                to = y,
                length.out = x)
for(i in 1:h){
  if(i <= x){
    X.USGDP.path[i] <- sequence[i]
  } else{
    X.USGDP.path[i] <- sequence[x]
  }
}

X.USGDP.path

# path matrix
exomat.path <- cbind(TRGT.path, X.USGDP.path)
colnames(exomat.path) <- c('TRGT','X.USGDP')

# forecast
fc.var <- forecast(mod.var, h = h, dumvar = exomat.path)
plot(fc.var, include = 36)

fc.var$model$varresult






#####
# GDP growth forecast
GDP.M.FC <- ts(c(GDP.M,fc.var$forecast$GDP.M$mean), start = start(GDP.M), frequency = frequency(GDP.M))
plot(GDP.M.FC)
lines(GDP.M, col = 'blue')

# CPI inflation forecast
CPI.FC <- ts(c(CPI,fc.var$forecast$GDP.M$mean), start = start(CPI), frequency = frequency(CPI))
plot(CPI.FC)
lines(CPI, col = 'blue')

# Unemployment rate forecast
UNEMP.FC <- ts(c(UNEMP,fc.var$forecast$UNEMP$mean), start = start(UNEMP), frequency = frequency(UNEMP))
plot(UNEMP.FC)
lines(UNEMP, col = 'blue')















