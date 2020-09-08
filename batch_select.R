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
# STEP 2.5: Variable selection
#--------------------------------------

setwd('/home/xie00039/Documents/Metro_transit/data/cleaned_data/cleaned_daily')
load('raw_daily.Rdata')
source('/home/xie00039/Documents/Metro_transit/phase2_code/glmPBD.R')

form1 = formula(Uncover ~ Start_H + Garage + Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance + tmpfu + tmpfl + sknt + mslp + p01i, data=raw_daily)
form2 = formula(Uncover~ Start_H + Garage *( Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance ) + tmpfu + tmpfl + sknt + mslp + p01i, data=raw_daily)
form3 = formula(Uncover~ Start_H + Garage *( Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance + tmpfu + tmpfl + sknt + mslp + p01i), data=raw_daily)
form4 = formula(Uncover~ Start_H + Garage + Work + Duty_cat + Route_preference + Day + Age + Seniority + Gender + Distance + Month*( tmpfu + tmpfl + sknt + mslp + p01i), data=raw_daily)
form5 = formula(Uncover~ Start_H + Garage *( Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance ) + Month * (tmpfu + tmpfl + sknt + mslp + p01i), data=raw_daily)

form.list = list(form1=form1,form2=form2,form3=form3,form4=form4,form5=form5)

for (i in 1:5){
  form = form.list[[i]]
  formatData = formatPBD(form=form, data=raw_daily)
  Y.list = formatData$Y.list
  X.list = formatData$X.list
  Y.panel = formatData$Y.panel
  model = glmPBDoptim(Y.list,X.list,verbose=F)
  pred = predictPBD(X.list,model)
  print(evalPBD(Y.list,pred))
}

# 1 Full model
# Uncover~ Start_H + Garage + Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance + tmpfu + tmpfl + sknt + mslp + p01i, data=raw_daily
# 

# 2 Full model + garage_interaction
# Uncover~ Start_H + Garage *( Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance ) + tmpfu + tmpfl + sknt + mslp + p01i, data=raw_daily
# 

# 3 Full model + garage_interaction_extended
# Uncover~ Start_H + Garage *( Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance + tmpfu + tmpfl + sknt + mslp + p01i), data=raw_daily
#

# 4 Full interaction + month_interaction
# Uncover~ Start_H + Garage + Work + Duty_cat + Route_preference + Day + Age + Seniority + Gender + Distance + Month*( tmpfu + tmpfl + sknt + mslp + p01i), data=raw_daily
#

# 5 Full interaction + garage_interaction + month_interaction
# Uncover~ Start_H + Garage *( Work + Duty_cat + Route_preference + Day + Month + Age + Seniority + Gender + Distance ) + Month * (tmpfu + tmpfl + sknt + mslp + p01i), data=raw_daily
# 

