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
# STEP 3: Model training
#--------------------------------------

setwd('/home/xie00039/Documents/Metro_transit/data/raw_data/schedule_data')
form = formula(Uncover~ Start_H + Garage + Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance + tmpfu + tmpfl + sknt + mslp + p01i, data=raw_daily)
zipcode = read.csv('free-zipcode-database-Primary.csv',header=T)
raw_operator = read.xlsx('Operators_Masked.xlsx',sheet=1,startRow=1)
route_preference = read.xlsx('Route_preference.xlsx',sheet=1)
route_class = read.xlsx('2016_2019_bus_route_class.xlsx',sheet=1)

setwd('/home/xie00039/Documents/Metro_transit/data/cleaned_data/cleaned_daily')
load('raw_daily.Rdata')
load('formatData.Rdata')
source('/home/xie00039/Documents/Metro_transit/phase2_code/glmPBD.R')

Y.list = formatData$Y.list
X.list = formatData$X.list
Y.panel = formatData$Y.panel

system.time({
  model = glmPBDoptim(Y.list,X.list,verbose=T)
})

level.list = list(Garage=levels(raw_daily$Garage),Gender=levels(raw_daily$Gender),Start_int=levels(raw_daily$Start_int),
               End_int=levels(raw_daily$End_int),Distance=levels(raw_daily$Distance),Month=levels(raw_daily$Month),
               Day=levels(raw_daily$Day),Route_class=levels(raw_daily$Route_class),Start_H=levels(raw_daily$Start_H),End_H=levels(raw_daily$End_H),Duty_cat=levels(raw_daily$Duty_cat))
model_info = list(model=model,raw_operator=raw_operator,zipcode=zipcode,level.list=level.list,route_preference=route_preference,route_class=route_class,form=form)
save(model_info,file='model_info.Rdata')
