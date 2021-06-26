#----------------------------------------------------#
#-----Policy Agendas and Markets - Market Data ------#
#----------------------------------------------------#

#This script gets stock market data from Yahoo Finance using the quantmod package. 
#The stock price data is processed to obtain monthly return and volatility data. 
#The monthly return and volatility data is then combined with the policy data. 


#### Packages and working directory ####

if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(zoo)) install.packages("zoo")
if(!require(lessR)) install.packages("lessR")
if(!require(TSstudio)) install.packages("TSstudio")
if(!require(quantmod)) install.packages("quantmod")


library(rstudioapi)
library(zoo)
library(lessR)
library(TSstudio)
library(quantmod)

this_files_path <- getActiveDocumentContext()$path
setwd(dirname(this_files_path))


#### S&P500 Daily Price Data ####

#Set dates and tickers
first.date <- Sys.Date() - (30*365)
last.date <- Sys.Date()

tickers <- c('^GSPC')

#Get the daily price data from Yahoo Finance for the ticker(s) and the time window selected above

l.out <- as.data.frame(getSymbols(tickers, src = "yahoo", from = first.date, to = last.date, auto.assign = FALSE))

#Get the closing prices from the above dataset and define as a new data frame

sp500 = l.out[,c('GSPC.Close', 'GSPC.Volume')]

#Extract month and year information from the date
sp500$date = as.Date(rownames(sp500))
sp500$month <- as.numeric(format(sp500$date, "%m"))
sp500$year <- as.numeric(format(sp500$date, "%Y"))

# Calculate daily log returns

sp500$return <- NA
for (i in 2:nrow(sp500)) {
    sp500$return[i] <- log(sp500$GSPC.Close[i]/sp500$GSPC.Close[i-1])
}

#Load the aam.data if it does not exist in the environment

if(!exists('aam.data')){
    source('policy.data.R')
}

#Calculate monthly volatility from daily log returns and append to the aam.data

for(i in 1:nrow(aam.data)){
    subset.data <- na.omit(subset(sp500, month == aam.data$month[i] & year == aam.data$year[i]))
    aam.data$h.m.volatility[i] <- sqrt(252)*100*sd(subset.data$return)
}

#### Daily VIX (Implied volatility Index) Data ####

#Set tickers
tickers <- c('^VIX')

l.out.vix <- as.data.frame(getSymbols(tickers, src = "yahoo", from = first.date, to = last.date, auto.assign = FALSE))

vix <- na.omit(l.out.vix[,c('VIX.Close', 'VIX.Volume')])

vix$date = as.Date(rownames(vix))
vix$month <- as.numeric(format(vix$date, "%m"))
vix$year <- as.numeric(format(vix$date, "%Y"))

#Calculate monthly average from daily closing prices and append to the aam.data

for(i in 1:nrow(aam.data)){
    subset.data <- na.omit(subset(vix, month == aam.data$month[i] & year == aam.data$year[i]))
    aam.data$vix.mean[i] <- mean(subset.data$VIX.Close)
}

#### Omit rows with missing values from the aam.data ####

# The resulting dataset will have all months and years between 6/1991 and 11/2015. 
#November 2015 doesn't have complete congressional hearings data. 
aam.data <- na.omit(aam.data[-nrow(aam.data),])




