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
# STEP 1: Clean historic data (2 year)
#--------------------------------------

# combine raw_daily data
setwd('/home/xie00039/Documents/Metro_transit/data/raw_data/schedule_data')
modified_frame = read.xlsx('2017_EMG_Masked_modified_labels.xlsx',sheet=1)
modified_name = colnames(modified_frame)
file_list = c("2017_EMG_Masked.xlsx"                                
              ,"2017_FTH_Masked.xlsx"                                
              ,"2017_MJR_Masked.xlsx"                                
              ,"2017_NIC_Masked.xlsx"                                
              ,"2017_SOU_Masked.xlsx"
              ,"2018_EMG_Masked.xlsx"                                
              ,"2018_FTH_Masked.xlsx"                                
              ,"2018_MJR_Masked.xlsx"                                
              ,"2018_NIC_Masked.xlsx"                                
              ,"2018_SOU_Masked.xlsx")
base_list = c(43180,43150,43140,43120,43160,43180,43150,43140,43120,43160)
raw_daily = read.xlsx(file_list[1],sheet=1)
raw_daily$empb_base_division = base_list[1]
for (i in 2:length(file_list)){
  temp_daily = read.xlsx(file_list[i],sheet=1)
  temp_daily$empb_base_division = base_list[i]
  raw_daily = rbind(raw_daily,temp_daily)
  print(file_list[i])
}
colnames(raw_daily) = modified_name

# merge raw_absence into raw_daily
raw_absence = read.xlsx('Absenteeism Occurrences 2017 and 2018 Formatted.xlsx',sheet=1,startRow=2)
raw_absence = raw_absence[(raw_absence$End.Date-raw_absence$Start.Date)<=60,]
raw_absence$cday = raw_absence$End.Date - raw_absence$Start.Date
raw_absence1 = data.frame(matrix(nrow=sum(raw_absence$cday)+nrow(raw_absence),ncol=3))
colnames(raw_absence1) = c('Employee','Date','Description')
raw_absence1$Employee = rep(raw_absence$Employee,raw_absence$cday+1)
raw_absence1$Description = rep(raw_absence$Description,raw_absence$cday+1)
absence_seq = function(X){
  return(seq(from=X[6],to=X[7]))
}
raw_absence1$Date = as.vector(unlist(apply(raw_absence,1,absence_seq)))
raw_daily = merge(raw_daily,raw_absence1,by.x=c('Original_employee','Date'),by.y=c('Employee','Date'),all.x=TRUE)

# merge raw_operator
raw_daily[is.na(raw_daily$Employee),]$Employee = raw_daily[is.na(raw_daily$Employee),]$Original_employee
raw_daily = raw_daily[!is.na(raw_daily$Employee),]
raw_daily = raw_daily[!is.na(raw_daily$Original_employee),]
raw_daily = raw_daily[raw_daily$Activity=='Operator',]
#raw_operator_old = read.xlsx('Operators_Masked.xlsx',sheet=1,startRow=1)
#raw_operator_new = read.xlsx('All Employees 4.21.20_DemographicData.xlsx',sheet=1,startRow=1)
#colnames(raw_operator_new) = c('employee_id','zipcode','birthdate','seniority_date','garage_id','start_date','gender')
#raw_operator_new$start_date = NULL
#raw_operator_new = raw_operator_new[,match(colnames(raw_operator_new),c('employee_id','zipcode','gender','birthdate','seniority_date','garage_id'))]
#raw_operator = rbind(raw_operator_old,raw_operator_new)
raw_operator = read.xlsx('Operators_Masked.xlsx',sheet=1,startRow=1)
raw_daily = merge(raw_daily,raw_operator,by.x=c('Original_employee'),by.y=c('employee_id'),all.x=TRUE)
raw_daily$garage_id = NULL

# focus on unplanned absences ('sick' belongs to planned absence)
raw_daily = raw_daily[!raw_daily$Description%in%c('Sick','Vacation Single Day','Floating Holiday','Left Service',
                                                  'Leave of Absence','PTO Vacation Week','Non-rush Request Off','OOA (Time Off)',
                                                  'Request Off (unpaid)','Birthday','Military Leave','Union Duties (ATU 1005)',
                                                  'Anniversary','Jury Duty'),]

# basic data cleaning
raw_daily = raw_daily[order(raw_daily$Date),]
raw_daily$Date = convertToDate(raw_daily$Date)
raw_daily$birthdate = convertToDate(raw_daily$birthdate)
raw_daily$Start = convertToDateTime(raw_daily$Start)
raw_daily$End = convertToDateTime(raw_daily$End)
true_value = c(43180,43150,43140,43120,43160)
name_value = c('EMG','FTH','MJR','NIC','SOU')
raw_daily$Garage = name_value[match(raw_daily$Garage,true_value)]
true_value = c("TYPE01","TYPE02","TYPE03","TYPE04","TYPE05","TYPE06","TYPE07","TYPE08","TYPE10","TYPE11",
               "TYPE12","TYPE13","TYPE20","TYPE99")
name_value = c("FT8 - bus","FTxboard AM","FTVacHolDown - bus","PT Weekday Non-Rostered - bus","PT Weekend Non-Rostered",
               "FT Miscellaneous","PT Weekday Student","PT Weekend Student","FT 10 - bus","FT 9 - bus","PT Weekday Rostered","PT Weekday Rostered Ten Reports","FTxboard PM","PASS Employee")
raw_daily$Operator_type = name_value[(match(raw_daily$Operator_type,true_value))]
raw_daily$Start_H = hour(raw_daily$Start)
raw_daily$Start_M = minute(raw_daily$Start)
raw_daily$End_H = hour(raw_daily$End)
raw_daily$Day = weekdays(as.Date(raw_daily$Date,origin='1970-01-01'))
raw_daily = raw_daily[raw_daily$Activity%in%c('Operator','Absence'),]
raw_daily$Month = month(raw_daily$Date)
raw_daily$Time_part = ifelse(raw_daily$Start_H<10,'AM','PM')
raw_daily$Time_part[raw_daily$Start_H==10] = ifelse(raw_daily$Start_M[raw_daily$Start_H==10]<=30,'AM','PM')
raw_daily$Time_part[raw_daily$Garage=='MJR'] = ifelse(raw_daily$Start_H[raw_daily$Garage=='MJR']<10,'AM','PM')
raw_daily$Duty_cat = ifelse(substr(raw_daily$Duty,2,4)%in%substr(as.character(seq(from=1001,to=1349,by=1)),2,4),'Run','Not run')

# advanced data cleaning (in another words, might be problematic) & feature engineering
raw_daily$Duty = NULL
raw_daily$From = NULL
raw_daily$To = NULL
raw_daily$ActType = NULL
raw_daily$wdy_call_duration = NULL
raw_daily$wdy_spread_dura = NULL
raw_daily$wdy_xb_utilization = NULL
raw_daily$Action = NULL
raw_daily$Time.stamp = NULL
raw_daily$Working = NULL
raw_daily$Activity = NULL
raw_daily$emp_yos_decimal = NULL
raw_daily$Start = NULL
raw_daily$Start_M = NULL
raw_daily$End = NULL
raw_daily$Description = NULL
raw_daily$Full_part = NULL

time_int = function(x,refer=c(6,12,18,24)){
  if (x<=refer[1]){
    return(paste(c('(0,',refer[1],']'),collapse=''))
  }
  if ((x>=refer[1])&(x<=refer[2])){
    return(paste(c('(',refer[1],',',refer[2],']'),collapse=''))
  }
  if ((x>=refer[2])&(x<=refer[3])){
    return(paste(c('(',refer[2],',',refer[3],']'),collapse=''))
  }
  if (x>=refer[3]){
    return(paste(c('(',refer[3],',24]'),collapse=''))
  }
}
raw_daily$Start_int = apply(as.matrix(raw_daily$Start_H),1,FUN=time_int)
raw_daily$End_int = apply(as.matrix(raw_daily$End_H),1,FUN=time_int)

raw_daily$Age = year(raw_daily$Date) - year(raw_daily$birthdate)
raw_daily$Seniority = year(raw_daily$Date) - year(convertToDate(raw_daily$seniority_date))
raw_daily$seniority_date = NULL
raw_daily$birthdate = NULL
raw_daily$Gender = raw_daily$gender
raw_daily$gender = NULL

route_preference = read.xlsx('Route_preference.xlsx',sheet=1)
route_class = read.xlsx('2016_2019_bus_route_class.xlsx',sheet=1)
route_class$Name = as.character(route_class$Name)
raw_daily = raw_daily[!is.na(raw_daily$Route),]
route_list = strsplit(raw_daily$Route,split='/')
date_list = as.numeric(raw_daily$Date)
processRoute = function(i,route_list,date_list,route_preference,route_class){
  route_vec = route_list[[i]]
  preference = min(route_preference$Preference[match(route_vec,route_preference$Route)])

  class_vec = c()
  k = 1
  for (j in 1:length(route_vec)){
    route =  route_vec[j]
    route_panel = route_class[route_class$Name==route,]
    class_id = sum(as.numeric(convertToDate(route_panel$'Date.Stamp'))<=as.numeric(date_list[i]))
    if (class_id!=0){
      class_vec[k] = route_panel[class_id,3]
      k = k + 1
    }
  }
  if ('LRT'%in%class_vec){
  class='LRT'}else{
    if ('CommRail'%in%class_vec){
    class='CommRail'}else{class=class_vec[1]}
  }
  class = paste(class,collapse='/')
  return(c(preference,class))
}

system.time({route_info = apply(as.matrix(1:length(date_list)),1,FUN=processRoute,route_list=route_list,date_list=date_list,route_preference=route_preference,route_class=route_class)})
route_info = as.data.frame(t(route_info))
colnames(route_info) = c('Preference','Class')
raw_daily$Route_preference = as.numeric(route_info$Preference)
raw_daily$Route_class = route_info$Class

zipcode = read.csv('free-zipcode-database-Primary.csv',header=T)
garage.panel = data.frame(garage=c('FTH','NIC','EMG','SOU','MJR'),zipcode=c('55411','55408','55117','55423','55430'),long=0,lat=0)
garage.panel$long = zipcode$Long[match(garage.panel$zipcode,zipcode$Zipcode)]
garage.panel$lat = zipcode$Lat[match(garage.panel$zipcode,zipcode$Zipcode)]
garage.long = garage.panel$long[match(raw_daily$Garage,garage.panel$garage)]
garage.lat = garage.panel$lat[match(raw_daily$Garage,garage.panel$garage)]
ind.long = zipcode$Long[match(raw_daily$zipcode,zipcode$Zipcode)]
ind.lat = zipcode$Lat[match(raw_daily$zipcode,zipcode$Zipcode)]
distance = sqrt((garage.long-ind.long)^2+(garage.lat-ind.lat)^2)
raw_daily$Distance = ifelse(distance==0,0,1)
raw_daily$zipcode = NULL

weather = riem_measures(station='MSP',date_start='2017-03-03',date_end='2019-03-09')
weather = weather[!is.na(weather$tmpf),]
weather$valid = as.Date(weather$valid,origin='1970-01-01')
weather.tmpfu = aggregate(weather$tmpf,by=list(Date=weather$valid),FUN=max)
weather.tmpfl = aggregate(weather$tmpf,by=list(Date=weather$valid),FUN=min)
weather.sknt = aggregate(weather$sknt,by=list(Date=weather$valid),FUN=mean,na.rm=T)
weather.mslp = aggregate(weather$mslp,by=list(Date=weather$valid),FUN=mean,na.rm=T)
weather.p01i = aggregate(weather$p01i,by=list(Date=weather$valid),FUN=mean,na.rm=T)
weather.panel = data.frame(Date=unique(weather$valid),tmpfu=weather.tmpfu[,2],tmpfl=weather.tmpfl[,2],
                           sknt=weather.sknt[,2],mslp=weather.mslp[,2],p01i=weather.p01i[,2])
weather.panel$Date = weather.panel$Date + 1
raw_daily = merge(raw_daily,weather.panel,by.x=c('Date'),by.y=c('Date'),all.x=TRUE)

raw_daily$Uncover = ifelse(raw_daily$Original_employee==raw_daily$Employee,0,1)
raw_daily$Work = as.integer(raw_daily$Work)
raw_daily$Platform = as.integer(raw_daily$Platform)
raw_daily$Overtime = as.factor(raw_daily$Overtime)
raw_daily$Planned.Off = as.factor(raw_daily$Planned.Off)
raw_daily$Garage = as.factor(raw_daily$Garage)
raw_daily$Operator_type = as.factor(raw_daily$Operator_type)
raw_daily$Day = as.factor(raw_daily$Day)
raw_daily$Start_int = as.factor(raw_daily$Start_int)
raw_daily$End_int = as.factor(raw_daily$End_int)
raw_daily$Age = as.integer(raw_daily$Age)
raw_daily$Gender = as.factor(raw_daily$Gender)
raw_daily$Distance = as.factor(raw_daily$Distance)
raw_daily$Month = as.factor(raw_daily$Month)
raw_daily$Time_part = as.factor(raw_daily$Time_part)
raw_daily$Duty_cat = as.factor(raw_daily$Duty_cat)
raw_daily$Start_H = as.factor(raw_daily$Start_H)
raw_daily$End_H = as.factor(raw_daily$End_H)
raw_daily = raw_daily[complete.cases(raw_daily),]

setwd('/home/xie00039/Documents/Metro_transit/data/cleaned_data/cleaned_daily')
save(raw_daily,file='raw_daily.Rdata')

head(raw_daily)



