# basic set up 
library(openxlsx)
library(EnvStats)
library(lubridate)
library(ROCR)
library(pROC)
library(MASS)
library(data.table)
library(riem)
library(ROpenWeatherMap)
library(numbers)

#--------------------------------------
# STEP 2: Model testing (Last 30 days of data)
#--------------------------------------
setwd('/home/xie00039/Documents/Metro_transit/data/cleaned_data/cleaned_daily')
load('formatData.Rdata')
load('raw_daily.Rdata')
source('/home/xie00039/Documents/Metro_transit/phase2_code/glmPBD.R')

Y.list = formatData$Y.list
X.list = formatData$X.list
Y.panel = formatData$Y.panel

test.ind = which((as.Date(Y.panel$Date,origin='1970-01-01')>=as.Date('2019-02-08',origin='1970-01-01')&
                   (as.Date(Y.panel$Date,origin='1970-01-01')<=as.Date('2019-03-08',origin='1970-01-01'))))
Y.train = Y.list[-test.ind]
Y.test = Y.list[test.ind]
X.train = X.list[-test.ind]
X.test = X.list[test.ind]

model = glmPBDoptim(Y.train,X.train,verbose=F)
out = predictPBD(X.test,model,upper.tail=0.9)
panel = Y.panel[test.ind,]
panel$mu = out$mu
panel$upw = out$upw

panel[
  with(panel, order(Date, Garage, Time_part, Duty_cat)),
]

sqrt(mean((panel$mu-panel$x)^2))
mean(ifelse(round(panel$mu)>=panel$x,1,0))
