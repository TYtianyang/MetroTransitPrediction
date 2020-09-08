preprocess = function(data,raw_operator,route_class,route_preference,zipcode,level.list,new_operator=NULL){
  
  # Step 1: Read the data & modify names
  data = data[,match(c('wdy_division','wdy_date','demp_display_id','pce_duration','wdy_is_set_plan_off'
                       ,'pce_is_ot','pce_status','pce_route_list','pce_place_start','pce_place_end'
                       ,'pce_time_start','pce_time_end','dpce_eff_duty'),colnames(data),)]
  colnames(data) = c('Garage','Date','Original_employee','Work','Planned.Off','Overtime','Status','Route','From','To','Start','End','Duty')
  
  # Step 2: Delete 'uncovered' and 'NA' in Status
  data = data[data$Status%in%c('COVERED','OVERTIME'),]
  
  # Step 3: Basic data cleaning
  true_value = c(43180,43150,43140,43120,43160)
  name_value = c('EMG','FTH','MJR','NIC','SOU')
  data$Garage = name_value[match(data$Garage,true_value)]
  data$Date = convertToDate(data$Date)
  data$Start = convertToDateTime(data$Start)
  data$End = convertToDateTime(data$End)
  data$Start_H = hour(data$Start)
  data$Start_M = minute(data$Start)
  data$End_H = hour(data$End)
  data$Time_part = ifelse(data$Start_H<10,'AM','PM')
  data$Time_part[data$Start_H==10] = ifelse(data$Start_M[data$Start_H==10]<=30,'AM','PM')
  data$Duty_cat = ifelse(substr(data$Duty,2,4)%in%substr(as.character(seq(from=1001,to=1349,by=1)),2,4),'Run','Not run')
  
  # Step 4: Merge raw_operator
  raw_operator$garage_id = NULL

  if (!is.null(new_operator)){
    colnames(new_operator) = c('employee_id','zipcode','birthdate','seniority_date','garage_id','start_date','gender')
    new_operator$start_date = NULL
    new_operator = new_operator[,match(colnames(new_operator),c('employee_id','zipcode','gender','birthdate','seniority_date','garage_id'))]
    new_operator$garage_id = NULL
    raw_operator = rbind(new_operator,raw_operator[!raw_operator$employee_id%in%new_operator$employee_id,])
    }

  data = merge(data,raw_operator,by.x=c('Original_employee'),by.y=c('employee_id'),all.x=TRUE)
  data$birthdate = convertToDate(data$birthdate)
  data$Age = year(data$Date) - year(data$birthdate)
  data$Seniority = year(data$Date) - year(convertToDate(data$seniority_date))
  data$seniority_date = NULL
  data$Gender = data$gender
  data$gender = NULL
  data$birthdate = NULL
  
  # Step 5: Advanced data cleaning & feature engineering
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
  data$Start_int = apply(as.matrix(data$Start),1,FUN=time_int)
  data$End_int = apply(as.matrix(data$End),1,FUN=time_int)
  data$Start_M = NULL
  
  data$Planned.Off = NULL
  data$Overtime = NULL
  data$Status = NULL
  data$From = NULL
  data$To = NULL

  route_class$Name = as.character(route_class$Name)
  route_list = strsplit(data$Route,split='/')
  date_list = as.numeric(data$Date)
  processRoute = function(i,route_list,date_list,route_preference,route_class){
    route_vec = route_list[[i]]
    if (is.na(route_vec[1])){
      preference=-1
      class='TBD'}else{
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
      class = paste(class_vec[1],collapse='/')
    }
    return(c(preference,class))
  }

  route_info = apply(as.matrix(1:length(date_list)),1,FUN=processRoute,route_list=route_list,date_list=date_list,route_preference=route_preference,route_class=route_class)
  route_info = as.data.frame(t(route_info))
  colnames(route_info) = c('Preference','Class')
  route_info$Class[is.na(route_info$Preference)] = 'TBD'
  route_info$Preference[is.na(route_info$Preference)] = -1
  data$Route_preference = as.numeric(route_info$Preference)
  data$Route_class = route_info$Class
  data$Route_preference[data$Route_preference==c(-1)] = 50
  data$Route_class[data$Route_class=='TBD'] = ''
  
  garage.panel = data.frame(garage=c('FTH','NIC','EMG','SOU','MJR'),zipcode=c('55411','55408','55117','55423','55430'),long=0,lat=0)
  garage.panel$long = zipcode$Long[match(garage.panel$zipcode,zipcode$Zipcode)]
  garage.panel$lat = zipcode$Lat[match(garage.panel$zipcode,zipcode$Zipcode)]
  garage.long = garage.panel$long[match(data$Garage,garage.panel$garage)]
  garage.lat = garage.panel$lat[match(data$Garage,garage.panel$garage)]
  ind.long = zipcode$Long[match(data$zipcode,zipcode$Zipcode)]
  ind.lat = zipcode$Lat[match(data$zipcode,zipcode$Zipcode)]
  distance = sqrt((garage.long-ind.long)^2+(garage.lat-ind.lat)^2)
  data$Distance = ifelse(distance==0,0,1)
  data$zipcode = NULL
  
  #print(mean(ifelse(is.na(data$Age),1,0)))
  #print(mean(ifelse(is.na(data$Seniority),1,0)))
  #print(mean(ifelse(is.na(data$Gender),1,0)))
  #print(mean(ifelse(is.na(data$Distance),1,0)))

  data$Age[is.na(data$Age)] = round(mean(data$Age,na.rm=T))
  data$Seniority[is.na(data$Seniority)] = round(mean(data$Seniority,na.rm=T))
  data$Gender[is.na(data$Gender)] = names(which.max(table(data$Gender)))
  data$Distance[is.na(data$Distance)] = names(which.max(table(data$Distance)))
  
  data$Month = month(data$Date)
  data$Day = weekdays(data$Date)
  
  # Step 6: Merge weather information
  date_start = head(unique(sort(data$Date)),1)
  date_end = tail(unique(sort(data$Date)+1),1)
  weather = riem_measures(station='MSP',date_start=date_start-1,date_end=date_end)
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
  data = merge(data,weather.panel,by.x=c('Date'),by.y=c('Date'),all.x=TRUE)
  
  # Step 7: Output
  data$Garage = as.factor(data$Garage)
  data$Gender = as.factor(data$Gender)
  data$Start_int = as.factor(data$Start_int)
  data$End_int = as.factor(data$End_int)
  data$Distance = as.factor(data$Distance)
  data$Month = as.factor(data$Month)
  data$Day = as.factor(data$Day)
  data$Route_class = as.factor(data$Route_class)
  data$Start_H = as.factor(data$Start_H)
  data$Duty_cat = as.factor(data$Duty_cat)
  data$Work = as.numeric(data$Work)
  data$Route_preference = as.numeric(data$Route_preference)
  data$Age = as.numeric(data$Age)
  data$tmpfu = as.numeric(data$tmpfu)
  data$tmpfl = as.numeric(data$tmpfl)
  data$sknt = as.numeric(data$sknt)
  data$mslp = as.numeric(data$mslp)
  data$p01i = as.numeric(data$p01i)
  levels(data$Garage) = level.list$Garage
  levels(data$Gender) = level.list$Gender
  levels(data$Start_H) = level.list$Start_H
  levels(data$End_H) = level.list$End_H
  levels(data$Duty_cat) = level.list$Duty_cat
  levels(data$Start_int) = level.list$Start_int
  levels(data$End_int) = level.list$End_int
  levels(data$Distance) = level.list$Distance
  levels(data$Month) = level.list$Month
  levels(data$Day) = level.list$Day
  levels(data$Route_class) = level.list$Route_class
  levels(data$Month) = level.list$Month
  return(data)
}