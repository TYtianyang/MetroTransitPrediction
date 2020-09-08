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

setwd('/home/xie00039/Documents/Metro_transit/data/cleaned_data/cleaned_daily')
load('raw_daily.Rdata')

table(raw_daily$Route_class)
source('/home/xie00039/Documents/Metro_transit/phase2_code/glmPBD.R')
form = formula(Uncover~ Start_H + Garage + Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance + tmpfu + tmpfl + sknt + mslp + p01i, data=raw_daily)
formatData = formatPBD(form=form, data=raw_daily)
save(formatData,file='formatData.Rdata')

