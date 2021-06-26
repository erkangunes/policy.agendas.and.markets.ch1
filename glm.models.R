#-----------------------------------------------------------#
#----- Policy Agendas and Markets - Volatility Models ------#
#-----------------------------------------------------------#

#This script estimates models of monthly historical volatility and monthly average on the VIX.
#I use Gamma GLM with log link and Logged DV OLS models because the volatility variables are positive and continuous.
#The sandwich package is used to get robust standard errors. 
#The stargazer function generates readable regression tables in the console if the type argument is set to "text".


#### Packages and working directory ####

if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(stargazer)) install.packages("stargazer")
if(!require(sandwich)) install.packages("sandwich")

library(rstudioapi)
library(stargazer)
library(sandwich)

this_files_path <- getActiveDocumentContext()$path
setwd(dirname(this_files_path))

if(!exists('aam.data')){
    source('policy.data.R')
    source('market.data.R')
}


#### Historical volatility - Entropy models ####

## GLM (Gamma distributed errors with log link) ##

m.e.hv.glm <- glm(formula = h.m.volatility ~ 
                    entropy + log(total.attention) + 
                    lag(entropy, 1) + log(lag(total.attention, 1)) + 
                    lag(entropy, 2) + log(lag(total.attention, 2)) +
                    lag(entropy, 3) + log(lag(total.attention, 3)) + 
                    epu + lag(epu, 1) + lag(epu, 2) + lag(epu, 3) +
                    lag(h.m.volatility, 1) + lag(h.m.volatility, 1) + lag(h.m.volatility, 3) + 
                    factor(year) + factor(month), 
                family = Gamma(link = "log"), 
                data = aam.data)

m.e.vix.glm <- glm(formula = vix.mean ~ 
                   entropy + log(total.attention) + 
                   lag(entropy, 1) + log(lag(total.attention, 1)) + 
                   lag(entropy, 2) + log(lag(total.attention, 2)) +
                   lag(entropy, 3) + log(lag(total.attention, 3)) + 
                   epu + lag(epu, 1) + lag(epu, 2) + lag(epu, 3) +
                   lag(vix.mean, 1) + lag(vix.mean, 1) + lag(vix.mean, 3) + 
                   factor(year) + factor(month), 
               family = Gamma(link = "log"), 
               data = aam.data)
    
    
# Robust standard errors

cov.1         <- vcovHC(m.e.hv.glm, type = "HC1")
robust.se.1    <- sqrt(diag(cov.1))

cov.2         <- vcovHC(m.e.vix.glm, type = "HC1")
robust.se.2    <- sqrt(diag(cov.2))


#Results
stargazer(m.e.hv.glm, m.e.vix.glm,
          se        = list(robust.se.1, robust.se.2),
          omit.stat = "f",
          type = "text")



####OLS with Logged DV####
#USA

m.e.hv.ols <- lm(formula = h.m.volatility ~ 
                      entropy + log(total.attention) + 
                      lag(entropy, 1) + log(lag(total.attention, 1)) + 
                      lag(entropy, 2) + log(lag(total.attention, 2)) +
                      lag(entropy, 3) + log(lag(total.attention, 3)) + 
                      epu + lag(epu, 1) + lag(epu, 2) + lag(epu, 3) +
                      lag(h.m.volatility, 1) + lag(h.m.volatility, 1) + lag(h.m.volatility, 3) + 
                      factor(year) + factor(month), 
                 data = aam.data)

m.e.vix.ols <- lm(formula = vix.mean ~ 
                       entropy + log(total.attention) + 
                       lag(entropy, 1) + log(lag(total.attention, 1)) + 
                       lag(entropy, 2) + log(lag(total.attention, 2)) +
                       lag(entropy, 3) + log(lag(total.attention, 3)) + 
                       epu + lag(epu, 1) + lag(epu, 2) + lag(epu, 3) +
                       lag(vix.mean, 1) + lag(vix.mean, 1) + lag(vix.mean, 3) + 
                       factor(year) + factor(month),
                   data = aam.data)


# Robust standard errors

cov.3         <- vcovHC(m.e.hv.ols, type = "HC1")
robust.se.3    <- sqrt(diag(cov.3))

cov.4         <- vcovHC(m.e.vix.ols, type = "HC1")
robust.se.4    <- sqrt(diag(cov.4))


# Results
stargazer(m.e.hv.ols, m.e.vix.ols,
          se        = list(robust.se.3, robust.se.4),
          omit.stat = "f",
          type = "text")

#### Historical volatility - Herfindahl models ####

## GLM (Gamma distributed errors with log link) ##

m.h.hv.glm <- glm(formula = h.m.volatility ~ 
                      herfindahl + log(total.attention) + 
                      lag(herfindahl, 1) + log(lag(total.attention, 1)) + 
                      lag(herfindahl, 2) + log(lag(total.attention, 2)) +
                      lag(herfindahl, 3) + log(lag(total.attention, 3)) + 
                      epu + lag(epu, 1) + lag(epu, 2) + lag(epu, 3) +
                      lag(h.m.volatility, 1) + lag(h.m.volatility, 1) + lag(h.m.volatility, 3) + 
                      factor(year) + factor(month), 
                  family = Gamma(link = "log"), 
                  data = aam.data)

m.h.vix.glm <- glm(formula = vix.mean ~ 
                       herfindahl + log(total.attention) + 
                       lag(herfindahl, 1) + log(lag(total.attention, 1)) + 
                       lag(herfindahl, 2) + log(lag(total.attention, 2)) +
                       lag(herfindahl, 3) + log(lag(total.attention, 3)) + 
                       epu + lag(epu, 1) + lag(epu, 2) + lag(epu, 3) +
                       lag(vix.mean, 1) + lag(vix.mean, 1) + lag(vix.mean, 3) + 
                       factor(year) + factor(month), 
                   family = Gamma(link = "log"), 
                   data = aam.data)


# Robust standard errors

cov.5         <- vcovHC(m.h.hv.glm, type = "HC1")
robust.se.5    <- sqrt(diag(cov.5))

cov.6         <- vcovHC(m.h.vix.glm, type = "HC1")
robust.se.6    <- sqrt(diag(cov.6))


#Results
stargazer(m.h.hv.glm, m.h.vix.glm,
          se        = list(robust.se.5, robust.se.6),
          omit.stat = "f",
          type = "text")



####OLS with Logged DV####
#USA

m.h.hv.ols <- lm(formula = h.m.volatility ~ 
                     herfindahl + log(total.attention) + 
                     lag(herfindahl, 1) + log(lag(total.attention, 1)) + 
                     lag(herfindahl, 2) + log(lag(total.attention, 2)) +
                     lag(herfindahl, 3) + log(lag(total.attention, 3)) + 
                     epu + lag(epu, 1) + lag(epu, 2) + lag(epu, 3) +
                     lag(h.m.volatility, 1) + lag(h.m.volatility, 1) + lag(h.m.volatility, 3) + 
                     factor(year) + factor(month), 
                 data = aam.data)

m.h.vix.ols <- lm(formula = vix.mean ~ 
                      herfindahl + log(total.attention) + 
                      lag(herfindahl, 1) + log(lag(total.attention, 1)) + 
                      lag(herfindahl, 2) + log(lag(total.attention, 2)) +
                      lag(herfindahl, 3) + log(lag(total.attention, 3)) + 
                      epu + lag(epu, 1) + lag(epu, 2) + lag(epu, 3) +
                      lag(vix.mean, 1) + lag(vix.mean, 1) + lag(vix.mean, 3) + 
                      factor(year) + factor(month),
                  data = aam.data)


# Robust standard errors

cov.7         <- vcovHC(m.h.hv.ols, type = "HC1")
robust.se.7    <- sqrt(diag(cov.7))

cov.8         <- vcovHC(m.h.vix.ols, type = "HC1")
robust.se.8    <- sqrt(diag(cov.8))


# Results
stargazer(m.h.hv.ols, m.h.vix.ols,
          se        = list(robust.se.7, robust.se.8),
          omit.stat = "f",
          type = "text")

