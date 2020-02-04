# basic set up 
library(openxlsx)
library(EnvStats)
library(lubridate)
library(ROCR)
library(pROC)
library(MASS)
library(data.table)
library(pracma)
library(dlm)
library(bestNormalize)
library(forecast)
library(splines)
library(riem)
library(rpart)
library(randomForest)
library(e1071)
library(glmnet)

# STEP 1: Clean a bit the schedule data
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
raw_daily = raw_daily[order(raw_daily$Date),]
raw_daily$Date = convertToDate(raw_daily$Date)
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
raw_daily$End_H = hour(raw_daily$End)
raw_daily$Day = weekdays(as.Date(raw_daily$Date,origin='1970-01-01'))

# STEP 2: Clean a bit the pick data
setwd('/home/xie00039/Documents/Metro_transit/data/raw_data/pick_data')
dir.list = c('2017-03','2017-06','2017-08','2017-12','2018-03','2018-06','2018-08','2018-12','2019-03')
modified.list = c('AUG18_MJR_Roster Positions masked.xlsx','AUG18_NIC_Roster Positions masked.xlsx',
              'AUG18_SOU_Roster Positions masked.xlsx','AUG18_EMG_Roster Positions masked.xlsx',
              'AUG18_FTH_Roster Positions masked.xlsx')
modified_name = colnames(read.xlsx('2018-08/AUG18_MJR_Roster Positions masked.xlsx',sheet=1))[-12]
raw_roster_list = list()
for (k in 1:length(dir.list)){
  dir.name = dir.list[k]
  file.list = list.files(dir.name)
  for (i in 1:length(file.list)){
    file.name = file.list[i]
    temp_roster = read.xlsx(paste(c(dir.name,'/',file.name),collapse=''),sheet=1)
    if (!file.name%in%modified.list){
      colnames(temp_roster) = modified_name
      temp_roster$Position = 2*temp_roster$Position + 63
    }else{
      temp_roster$Rsc = NULL
    }
    temp_roster$Garage = strsplit(file.name,split='_*_')[[1]][2]
    if (i==1){
      raw_roster = temp_roster
    }else{
      raw_roster = rbind(raw_roster,temp_roster)
    }
  }
  raw_roster = raw_roster[raw_roster$Roster%in%c('FT8','FT9','FT10'),]
  raw_roster = raw_roster[!grepl("^[A-Za-z]+$", raw_roster$Id, perl = T),]
  raw_roster$Start = convertToDateTime(raw_roster$Start)
  raw_roster$End = convertToDateTime(raw_roster$End)
  raw_roster$Day[!raw_roster$Day%in%c("Friday","Monday","Saturday","Sunday",   
                                      "Thursday","Tuesday","Wednesday")] = as.character(as.Date(raw_roster$Day[!raw_roster$Day%in%c("Friday","Monday","Saturday","Sunday",   
                                                                                                                                    "Thursday","Tuesday","Wednesday")],'%m/%d/%Y'))
  raw_roster$Start_H = hour(raw_roster$Start)
  raw_roster$End_H = hour(raw_roster$End)
  raw_roster_list[[dir.name]] = raw_roster
}


# STEP 3: Generate cleaned data for regular runs
subDaily = function(season,raw_daily){
  pickdates = as.Date(c("2017-03-04","2017-06-17","2017-08-19","2017-12-02","2018-03-17",
                        "2018-06-09","2018-08-18","2018-12-01","2019-03-09"),origin='1970-01-01')
  season_panel = data.frame(from=(pickdates+1)[-9],to=(pickdates-1)[-1])
  season_ind = c("2017-03","2017-06","2017-08","2017-12","2018-03","2018-06","2018-08","2018-12","2019-03")
  ind = match(season,season_ind)
  from = season_panel$from[ind]
  to = season_panel$to[ind]
  output = (raw_daily[(as.Date(raw_daily$Date,origin='1970-01-01')>=from)&
                     (as.Date(raw_daily$Date,origin='1970-01-01')<=to),])
  return(output)
}

dutyFilter = function(season,raw_daily,raw_roster_list){
  raw_roster = raw_roster_list[[season]]
  sub_daily = subDaily(season,raw_daily)
  dayduty.list = unique(as.character(interaction(raw_roster$Day,raw_roster$Id,drop=T,sep=';')))
  
  dutyfilter = function(i,dayduty.list,sub_daily,raw_roster){
    oneP = c('1P08','1P09','1P10','TR9')
    dayduty = dayduty.list[i]
    day = strsplit(dayduty,split=';',fixed=T)[[1]][1]
    duty = strsplit(dayduty,split=';',fixed=T)[[1]][2]
    if (unique(raw_roster[(raw_roster$Day==day)&(raw_roster$Id==duty),'Type'])%in%oneP){p=1
    }else{p=2}
    subsub_daily = sub_daily[((sub_daily$Day==day)|(as.character(sub_daily$Date)==day))&(sub_daily$Duty==duty),]
    startend = sort(table(as.character(interaction(subsub_daily$Start_H,subsub_daily$End_H,drop=T,sep=';')))
                  ,decreasing=T)
    startend = startend[1:min(p,length(startend))]
    fromto = sort(table(as.character(interaction(subsub_daily$From,subsub_daily$To,drop=T,sep=';')))
                        ,decreasing=T)
    fromto = fromto[1:min(p,length(fromto))]
    route = sort(table(subsub_daily$Routes,useNA='ifany'),decreasing=T)
    route = route[1:min(p,length(route))]
    if (p==1){
      start = strsplit(names(startend),split=';')[[1]][1]
      end = strsplit(names(startend),split=';')[[1]][2]
      from = strsplit(names(fromto),split=';')[[1]][1]
      to = strsplit(names(fromto),split=';')[[1]][2]
      route = names(route)[1]
    }else{
      start = c()
      end = c()
      from = c()
      to = c()
      if (length(startend)==2){
        if (startend[1]<=2*startend[2]){
          start[1] = strsplit(names(startend),split=';')[[1]][1]
          end[1] = strsplit(names(startend),split=';')[[1]][2]
          start[2] = strsplit(names(startend),split=';')[[2]][1]
          end[2] = strsplit(names(startend),split=';')[[2]][2]
        }else{
          start = rep(strsplit(names(startend),split=';')[[1]][1],2)
          end = rep(strsplit(names(startend),split=';')[[1]][2],2)
        }
      }else{
        start = rep(strsplit(names(startend),split=';')[[1]][1],2)
        end = rep(strsplit(names(startend),split=';')[[1]][2],2)
      }
      if (length(fromto)==2){
        if (fromto[1]<=2*fromto[2]){
          from[1] = strsplit(names(fromto),split=';')[[1]][1]
          to[1] = strsplit(names(fromto),split=';')[[1]][2]
          from[2] = strsplit(names(fromto),split=';')[[2]][1]
          to[2] = strsplit(names(fromto),split=';')[[2]][2]
        }else{
          from = rep(strsplit(names(fromto),split=';')[[1]][1],2)
          to = rep(strsplit(names(fromto),split=';')[[1]][2],2)
        }
      }else{
        from = rep(strsplit(names(fromto),split=';')[[1]][1],2)
        to = rep(strsplit(names(fromto),split=';')[[1]][2],2)
      }
      if (length(route)==1){
        route = rep(names(route),2) 
      }else{
        if (route[1]>2*route[2]){
          route = rep(names(route[1]),2)
        }else{
          route = names(route)
        }
      }
    }
    output = list(start=start,end=end,from=from,to=to,route=route)
    return(output)
  }
  
  # for (i in 1:length(dayduty.list)){
  #   temp.detail = dutyfilter(i,dayduty.list,sub_daily,raw_roster)
  # }
  
  system.time({
    detail.list = apply(as.matrix(c(1:length(dayduty.list))),1,FUN=dutyfilter,
                        dayduty.list=dayduty.list,raw_roster=raw_roster,sub_daily=sub_daily)
  })
  names(detail.list) = dayduty.list
  output = list(detail.list=detail.list,dayduty.list=dayduty.list)
  return(output)
}

panelConstruct = function(season,raw_daily,raw_roster_list){
  raw_roster = raw_roster_list[[season]]
  sub_daily = subDaily(season,raw_daily)
  dayduty.list = unique(as.character(interaction(raw_roster$Day,raw_roster$Id,drop=T,sep=';')))
  date.list = names(table(sub_daily$Date))
  
  SingleDate = function(Date,raw_roster){
    special_date = as.Date(names(table(raw_roster$Day))[!names(table(raw_roster$Day))%in%
                                                          c("Friday","Monday","Saturday","Sunday",   
                                                            "Thursday","Tuesday","Wednesday")],'%Y-%m-%d')
    Date = as.Date(Date,'%Y-%m-%d')
    day = weekdays(Date)
    if (Date%in%special_date){
      day.ind=as.character(Date)
    }else{
      day.ind=day
    }
    sub_roster = raw_roster[raw_roster$Day==day.ind,]
    
    oneP = c('1P08','1P09','1P10','TR9')
    repV = ifelse(sub_roster$Type%in%oneP,1,2)
    numR = sum(repV)
    output = matrix(ncol=16,nrow=numR)
    output = as.data.frame(output)
    colnames(output) = c('Date','Weekday','Holiday','Garage','Duty type','Duty id','1p/2p','Original operator','Actual operator'
                         ,'Start','End','From','To','Duration','Routes'
                         ,'Uncover')
    output$Date = as.character(Date)
    output$Weekday = day
    output$Holiday = day.ind
    output[,'Garage'] = rep(sub_roster$Garage,repV)
    output[,'Duty type'] = rep(sub_roster$Roster,repV)
    output[,'Duty id'] = rep(sub_roster$Id,repV)
    output[,'1p/2p'] = rep(sub_roster$Type,repV)
    output[,'Original operator'] = rep(sub_roster$Position,repV)
    return(output)
  }
  for (i in 1:length(date.list)){
    date = date.list[i]
    panel = SingleDate(date,raw_roster)
  }
  
  panel.list = apply(as.matrix(date.list),1,FUN=SingleDate,raw_roster=raw_roster)
  for (i in 1:length(date.list)){
    if (i==1){
      panel=panel.list[[i]]
    }else{
      panel=rbind(panel,panel.list[[i]])
    }
  }
  return(panel)
}

panelFill = function(panel,detail.list){
  
  singleFill = function(dayduty,detail.list){
    detail = detail.list[[dayduty]]
    temp.panel = data.frame(matrix(nrow=length(detail$start),ncol=5))
    names(temp.panel) = c('Start','End','From','To','Routes')
    temp.panel$Start = detail$start
    temp.panel$End = detail$end
    temp.panel$From = detail$from
    temp.panel$To = detail$to
    temp.panel$Routes = detail$route
    return(temp.panel)
  }
  dayduty.col.raw = as.character(interaction(panel$Holiday,panel$`Duty id`,drop=T,sep=';'))
  dayduty.col = c()
  for (i in 1:length(dayduty.col.raw)){
    if (i==1){
      temp.dayduty = dayduty.col.raw[i]
      dayduty.col[i] = temp.dayduty
    }else{
      if (dayduty.col.raw[i]!=temp.dayduty){
        temp.dayduty = dayduty.col.raw[i]
        dayduty.col[i] = temp.dayduty
      }
    }
  }
  dayduty.col = na.omit(dayduty.col)
  temp.panel.list = apply(as.matrix(dayduty.col),1,FUN=singleFill,detail.list=detail.list)
  detail.panel = rbindlist(temp.panel.list,use.names=T,fill=T)
  panel[,c('Start','End','From','To','Routes')] = detail.panel
  panel$Duration = round(ifelse(as.numeric(panel$Start)<=as.numeric(panel$End)
                          ,as.numeric(panel$End)-as.numeric(panel$Start)
                          ,24-as.numeric(panel$Start)+as.numeric(panel$End)),0)
  
  return(panel)
}

seekActual = function(panel,season,raw_daily){
  
  Sub_daily = subDaily(season,raw_daily)
  dateduty.col.raw = as.character(interaction(panel$Date,panel$`Duty id`,drop=T,sep=';'))
  dateduty.col = unique(dateduty.col.raw)
  oneP = c('1P08','1P09','1P10','TR9')
  p.col = ifelse(panel$`1p/2p`[match(dateduty.col,dateduty.col.raw)]%in%oneP,1,2)
  OO.col = panel$`Original operator`[match(dateduty.col,dateduty.col.raw)]
  
  singleSeek = function(i,dateduty.col,p.col,OO.col,Sub_daily,panel){
    
    dateduty = dateduty.col[i]
    date = strsplit(dateduty,split=';',fixed=T)[[1]][1]
    duty = strsplit(dateduty,split=';',fixed=T)[[1]][2]
    p = p.col[i]
    OO = OO.col[i]
    sub_daily = Sub_daily[(Sub_daily$Date==date)&(Sub_daily$Duty==duty),]
    sub_panel = panel[(panel$Date==date)&(panel$`Duty id`==duty),]
  
    if (nrow(sub_daily)==0){
      AO = rep(NA,p)
    }else{
      sub_start = sub_daily$Start_H
      sub_end = sub_daily$End_H
      sub_employee = sub_daily$Employee
      
      if (p==1){
        duration = ifelse(sub_start<=sub_end,sub_end-sub_start
                          ,24-sub_start+sub_end)
        AO = ifelse(sum(sub_employee!=OO)>=1
                    ,sub_employee[sub_employee!=OO][1]
                    ,OO)
      }else{
        if (nrow(sub_daily)>1){
          start = as.integer(sub_panel$Start)
          end = as.integer(sub_panel$End)
          start.1 = start[1]
          start.2 = start[2]
          end.1 = end[1]
          end.2 = end[2]
          score.1 = (as.integer(sub_start) - start.1)^2 + (as.integer(sub_end) - end.1)^2
          score.2 = (as.integer(sub_start) - start.2)^2 + (as.integer(sub_end) - end.2)^2
          AO = c(sub_employee[which.max(score.1)],sub_employee[which.max(score.2)])
        }else{
          AO = c(sub_employee,NA)
        }
      }
    }
    
    return(AO)
  }
  
  system.time({
    AO.list = apply(as.matrix(c(1:length(p.col))),1,FUN=singleSeek,
                           dateduty.col=dateduty.col,p.col=p.col,OO.col=OO.col,
                           Sub_daily=Sub_daily,panel=panel)
  })
  
  return(unlist(AO.list))
  
}

cleanPanel = function(season,raw_daily,raw_roster_list){
  
  # step 1: panelConstruct(),construct clean panel, NA in detailed information
  panel = panelConstruct(season,raw_daily,raw_roster_list)
  
  # step 2: dutyFilter(), collect time,location,route information in each duty:day pair
  temp = dutyFilter(season,raw_daily,raw_roster_list)
  detail.list = temp$detail.list
  dayduty.list = temp$dayduty.list
  
  # step 3: panelFill(), fill in time,location,route information in each duty:day pair
  panel = panelFill(panel,detail.list)
  
  # step 4: seekActual(), fit in the actual operator, by scoring technique that defined on distance to time
  panel$`Actual operator` = seekActual(panel,season,raw_daily)
  
  # step 5: generate uncover
  panel$Uncover = ifelse(((is.na(panel$`Actual operator`))|
                            (panel$`Actual operator`!=panel$`Original operator`)),1,0)
  
  return(panel)
}

seasons = c('2017-03','2017-06','2017-08','2017-12','2018-03','2018-06','2018-08','2018-12')
for (i in 1:length(seasons)){
  season = seasons[i]
  cleanpanel = cleanPanel(season,raw_daily,raw_roster_list)
  write.csv(cleanpanel,file=paste(c(season,'.csv'),collapse=''))
}



# STEP 4: exploratory analysis
sum.list = list()
garage.list = c('FTH','NIC','EMG','SOU','MJR')
for (i in 1:5){
  sum.list[[i]] = lookNSclean(clean,garage.list[i],c("2017-03-05","2017-05-31"),p=F,fit=FALSE)
}
plot(p~date,data=sum.list[[1]],type='l',xlab=paste(c("2018-08-20","2018-11-30"),collapse='   to   '),ylab='Prop.',ylim=c(0,0.4),
     main='Proportion of Uncovers in Regular Runs')
lines(p~date,data=sum.list[[2]],col='blue',lty=2)
lines(p~date,data=sum.list[[3]],col='red',lty=2)
lines(p~date,data=sum.list[[4]],col='green',lty=1)
lines(p~date,data=sum.list[[5]],col='orange',lty=1)
legend('bottomright',legend=garage.list,lty=c(1,2,2,1,1),col=c('black','blue','red','green','orange'))

temp.nduty = aggregate(clean$Uncover,list(c1=clean$Date,c2=clean$`Duty id`,c3=clean$Garage),function(X){return(1)})
temp.n0duty = aggregate(clean$Uncover,list(c1=clean$Date,c2=clean$`Duty id`,c3=clean$Garage),max)
temp.n = aggregate(temp.nduty$x,list(c1=temp.nduty$c1,c2=temp.nduty$c3),length)
temp.n0 = aggregate(temp.n0duty$x,list(c1=temp.n0duty$c1,c2=temp.nduty$c3),sum)
duty.panel = temp.n
duty.panel$u = temp.n0$x
id.names = names(table(duty.panel$c2))
for (i in 1:5){
  id = id.names[i]
  plot(y=duty.panel[duty.panel$c2==id,'x'],x=as.Date(duty.panel[duty.panel$c2==id,'c1'],
                                                     origin='1970-01-01'),col='black',type='l',main=id,
       ylim=c(0,max(duty.panel[duty.panel$c2==id,'x'])),xlab='Time',ylab='Counts')
  lines(y=duty.panel[duty.panel$c2==id,'u'],x=as.Date(duty.panel[duty.panel$c2==id,'c1'],
                                                      origin='1970-01-01'),col='black',lty=2)
}

  
# STEP 5: fitting
setwd('/home/xie00039/Documents/Metro_transit/data/cleaned_data/cleaned_seasonal')
seasons = c('2017-03','2017-06','2017-08','2017-12','2018-03','2018-06','2018-08','2018-12')
for (i in 1:length(seasons)){
  season = seasons[i]
  if (i==1){
    clean = read.csv(paste(c(season,'.csv'),collapse=''))
  }else{
    clean = rbind(clean,read.csv(paste(c(season,'.csv'),collapse='')))
  }
}
clean[,1] = NULL
clean = clean[!is.na(clean$Routes),]
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
replwd = function(date){
  if (date%in%c("Friday","Monday","Saturday","Sunday",   
                "Thursday","Tuesday","Wednesday")){
    return(date)
  }else{
    date = as.Date(date,origin='1970-01-01')
    if ((month(date)==1)&(day(date)==1)){
      return('01-01')
    }
    if ((month(date)==5)&(day(date)>=28)&(day(date)<=39)){
      return('05-28')
    }
    if ((month(date)==7)&(day(date)==4)){
      return('07-04')
    }
    if ((month(date)==9)&(day(date)>=3)&(day(date)<=4)){
      return('09-04')
    }
    if ((month(date)==11)&(day(date)>=22)&(day(date)<=24)){
      return('11-23')
    }
    if ((month(date)==12)&(day(date)>=24)&(day(date)<=25)){
      return('12-24')
    }
  }
}
clean$Start_int = apply(as.matrix(clean$Start),1,FUN=time_int)
clean$End_int = apply(as.matrix(clean$End),1,FUN=time_int)
oftenLOC = c('HEWD','EMET','SOUT','NICL','MJR')
clean$From_M = as.character(clean$From)
clean$From_M[!clean$From_M%in%oftenLOC] = 'OTHER'
clean$To_M = as.character(clean$To)
clean$To_M[!clean$To_M%in%oftenLOC] = 'OTHER'
pickdates = as.Date(c("2017-03-04","2017-06-17","2017-08-19","2017-12-02","2018-03-17",
                      "2018-06-09","2018-08-18","2018-12-01","2019-03-09"))
dates = as.Date(clean$Date,origin='1970-01-01')
indpanel = matrix(0,nrow=nrow(clean),ncol=9)
for (i in 1:9){
  indpanel[,i] = ifelse(dates>=pickdates[i],1,0)
}
indcol = rowSums(indpanel)
clean$timecum = as.numeric(dates - pickdates[indcol])
indpanel = matrix(0,nrow=length(dates),ncol=3)
for (i in 1:3){
  indpanel[,i] = ifelse(dates>=as.Date(c('2017-01-01','2018-01-01','2019-01-01'),origin='1970-01-01')[i],1,0)
}
indcol = rowSums(indpanel)
clean$datecum = as.numeric(dates - as.Date(c('2017-01-01','2018-01-01','2019-01-01'),origin='1970-01-01')[indcol])
clean$months = as.factor(month(dates))
clean$wd = as.factor(apply(as.matrix(clean$Holiday),1,FUN=replwd))
payday = seq.Date(from=as.Date('2017-03-03',origin='1970-01-01'),
                  to=as.Date('2019-03-09',origin='1970-01-01'),by=14)
lastpayday = function(i,dates,payday){
  date = as.Date(dates[i],origin='1970-01-01')
  lastpd = payday[max(which(payday<=date))]
  return(lastpd)
}
lastpds = apply(as.matrix(c(1:nrow(clean))),1,FUN=lastpayday,dates=clean$Date,payday=payday)
clean$pdcum = as.numeric(as.Date(clean$Date,origin='1970-01-01')) - lastpds
udates = unique(dates)
# tbs = function(date){
#   pickdates = as.Date(c("2017-03-04","2017-06-17","2017-08-19","2017-12-02",
#                         "2018-03-17","2018-06-09","2018-08-18","2018-12-01",
#                         "2019-03-09","2019-06-09","2019-08-18","2019-12-01"),origin='1970-01-01')
#   date.year = year(date)
#   months = as.Date(paste(date.year,c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01',
#                              '10-01','11-01','12-01'),sep='-'),origin='1970-01-01')
#   
#   refer.wd = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
#   for (i in 1:12){
#     month = months[i]
#     month.wd = weekdays(month)
#     months[i] = month - match(month.wd,refer.wd)
#   }
#   
#   timecut = as.Date(sort(union(months,pickdates[(1+(match(date.year,c(2017:2019))-1)*4):(match(date.year,c(2017:2019))*4)])),origin='1970-01-01')
#   output = pmax(as.numeric(date - timecut),0)
#   return(output)
# }
# tlist = list()
# for (i in 1:length(udates)){
#   udate = udates[i]
#   tlist[[i]] = tbs(udate)
# }
# tbmatch = function(date,udates,tlist){
#   return(tlist[[match(as.Date(date,origin='1970-01-01'),udates)]])
# }
# tpanel = apply(as.matrix(clean$Date),1,FUN=tbmatch,udates=udates,tlist=tlist)
# tpanel = t(tpanel)
# tpanel = as.data.frame(tpanel)
# colnames(tpanel) = paste('t',c(1:ncol(tpanel)),sep='')
# clean = cbind(clean,tpanel)
dateint = function(date){
  pickdates = as.Date(c("2017-03-04","2017-06-17","2017-08-19",
                        "2018-03-17","2018-06-09","2018-08-18",
                        "2019-03-09","2019-06-09","2019-08-18"),origin='1970-01-01')
  date.year = year(date)
  months = as.Date(paste(date.year,c('01-01','02-01','03-01','04-01','05-01','06-01','07-01','08-01','09-01',
                                     '10-01','11-01','12-01'),sep='-'),origin='1970-01-01')
  # refer.wd = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
  # for (i in 1:12){
  #   month = months[i]
  #   month.wd = weekdays(month)
  #   months[i] = month - match(month.wd,refer.wd)
  # }
  # timecut = as.Date(sort(union(months,pickdates[(1+(match(date.year,c(2017:2019))-1)*3):(match(date.year,c(2017:2019))*3)])),origin='1970-01-01')
  timecut = as.Date(pickdates[(1+(match(date.year,c(2017:2019))-1)*3):(match(date.year,c(2017:2019))*3)],origin='1970-01-01')
  output = sum(date>=timecut)
  return(output)
}
tlist = c()
for (i in 1:length(udates)){
  udate = udates[i]
  tlist[i] = dateint(udate)
}
tbmatch = function(date,udates,tlist){
  return(tlist[match(as.Date(date,origin='1970-01-01'),udates)])
}
tpanel = apply(as.matrix(clean$Date),1,FUN=tbmatch,udates=udates,tlist=tlist)
clean$tint = as.factor(tpanel)

clean$Holiday = as.factor(clean$Holiday)
clean$Weekday = as.factor(clean$Weekday)
clean$Start = as.factor(clean$Start)
clean$End = as.factor(clean$End)
clean$Start_int = as.factor(clean$Start_int)
clean$End_int = as.factor(clean$End_int)
clean$From_M = as.factor(clean$From_M)
clean$To_M = as.factor(clean$To_M)
clean$Garage = as.factor(clean$Garage)
clean$Routes = as.character(clean$Routes)

# train = clean[(as.Date(clean$Date,origin='1970-01-01')<as.Date('2018-12-01',origin='1970-01-01'))&
#                 (as.Date(clean$Date,origin='1970-01-01')>=as.Date('2018-03-03',origin='1970-01-01')),]
# test = clean[as.Date(clean$Date,origin='1970-01-01')>=as.Date('2018-12-01',origin='1970-01-01'),]
# 
# initial_m = glm(Uncover~ 1 + Duration+ Weekday*Garage + Start + End   ,data=train,family='binomial')
# test$fit = predict(initial_m,newdata=test,type='response')
# lookroc(test)
# 
# formula = formula(Uncover~ (1+ Duration |Routes)  + Weekday + Start_int + End_int  + timecum + Garage )
# initial_m = glm(Uncover~ 1 + Duration+ Weekday + Start_int + End_int  + timecum + Garage  ,data=train,family='binomial')
# m = CATCOO.fit(formula,train,id.names=NULL,t.max=100,epsi=0.05,initial_m=initial_m)
# test$fit = CATSGD.predict(m,test)
# train$fit = CATSGD.predict(m,train)
# lookroc(train)
# lookroc(test)
# 
# lambda = 0.255
# temp_test = test
# temp_test$fit = ifelse(test$fit>=lambda,1,0)
# temp = lookNSclean(temp_test,'NIC',c('2019-01-01','2019-03-03'),p=F,fit=T)

############################################
# Ind Bad Model
test = clean[as.Date(clean$Date,origin='1970-01-01')>=as.Date('2018-12-01',origin='1970-01-01'),]
train = clean[!as.Date(clean$Date,origin='1970-01-01')>=as.Date('2018-12-01',origin='1970-01-01'),]
m = glm(Uncover~Garage +
          wd + X1p.2p + Duty.type + Start_int * Duration + From_M + To_M + timecum + pdcum + months + tint,
        data=train,family='binomial')
test$pred = predict(m,newdata=test,type='response')
error = aggregate(test$Uncover,by=list(cat1=test$Date,cat2=test$Garage),sum)$x - 
  aggregate(test$pred,by=list(cat1=test$Date,cat2=test$Garage),sum)$x
c(mean(error^2),sqrt(var(error^2)/length(error)),max(error^2))

test = clean[as.Date(clean$Date,origin='1970-01-01')>=as.Date('2018-12-01',origin='1970-01-01'),]
train = clean[!as.Date(clean$Date,origin='1970-01-01')>=as.Date('2018-12-01',origin='1970-01-01'),]
m = glm(Uncover~bs(datecum,knots=c(25,70,95,130,195,215,230,250,280,330)) + Garage +
          wd + X1p.2p + Duty.type + Start_int * Duration + From_M + To_M + timecum + pdcum + months + tint,
        data=train,family='binomial')
test$pred = predict(m,newdata=test,type='response')
error = aggregate(test$Uncover,by=list(cat1=test$Date,cat2=test$Garage),sum)$x - 
  aggregate(test$pred,by=list(cat1=test$Date,cat2=test$Garage),sum)$x
c(mean(error^2),sqrt(var(error^2)/length(error)),max(error^2))

############################################
# New Models (Aggregated Response)
measures = riem_measures(station='MSP',date_start='2017-03-05',date_end='2019-03-09')
measures$date = as.Date(measures$valid,format='%Y-%m-%d')
dates = unique(measures$date)
tmpf = aggregate(measures$tmpf,by=list(date=measures$date),mean,na.rm=T)[,2]
dwpf = aggregate(measures$dwpf,by=list(date=measures$date),mean,na.rm=T)[,2]
relh = aggregate(measures$relh,by=list(date=measures$date),mean,na.rm=T)[,2]
sknt = aggregate(measures$sknt,by=list(date=measures$date),mean,na.rm=T)[,2]
p01i = aggregate(measures$p01i,by=list(date=measures$date),mean,na.rm=T)[,2]
vsby = aggregate(measures$vsby,by=list(date=measures$date),mean,na.rm=T)[,2]
feel = aggregate(measures$feel,by=list(date=measures$date),mean,na.rm=T)[,2]
weather = data.frame(date=dates,tmpf=tmpf,dwpf=dwpf,relh=relh,sknt=sknt,p01i=p01i,vsby=vsby,feel=feel)

garage.list = c('NIC','FTH','SOU','MJR','EMG')
panel.list = list()
for (k in 1:length(garage.list)){
  garage = garage.list[k]
  temp = lookNSclean(clean,garage,c('2017-01-01','2019-04-03'),p=F,fit=F)
  temp_y = temp$n0
  temp_n = temp$n
  temp_clean = clean[clean$Garage==garage,]
  dates = unique(as.Date(temp_clean$Date,origin='1970-01-01'))
  months = as.factor(month(dates))
  wd = temp_clean$Holiday[match(as.character(dates),as.character(temp_clean$Date))]
  wd = apply(as.matrix(wd),1,FUN=replwd)
  
  indpanel = matrix(0,nrow=length(dates),ncol=9)
  for (i in 1:9){
    indpanel[,i] = ifelse(dates>=pickdates[i],1,0)
  }
  indcol = rowSums(indpanel)
  timecum = as.numeric(dates - pickdates[indcol])
  indpanel = matrix(0,nrow=length(dates),ncol=3)
  for (i in 1:3){
    indpanel[,i] = ifelse(dates>=as.Date(c('2017-01-01','2018-01-01','2019-01-01'),origin='1970-01-01')[i],1,0)
  }
  indcol = rowSums(indpanel)
  datecum = as.numeric(dates - as.Date(c('2017-01-01','2018-01-01','2019-01-01'),origin='1970-01-01')[indcol])
  
  rStart = expand.grid(dates,c('(0,6]','(6,12]','(12,18]'))
  rStart$x = 0
  colnames(rStart) = c('Date','Start','x')
  rStart_temp = aggregate(temp_clean$Date,by=list(Date=temp_clean$Date,Start=temp_clean$Start_int),length)
  rStart_temp = rStart_temp[!rStart_temp$Start=='(18,24]',]
  rStart$x[match(as.character(interaction(rStart_temp$Date,rStart_temp$Start)),
                 as.character(interaction(rStart$Date,rStart$Start)))] = rStart_temp$x
  rN = aggregate(temp_clean$Date,by=list(Date=temp_clean$Date),length)[,2]
  rStart1 = rStart$x[rStart$Start=='(0,6]']/rN
  rStart2 = rStart$x[rStart$Start=='(6,12]']/rN
  rStart3 = rStart$x[rStart$Start=='(12,18]']/rN
  rEnd = expand.grid(dates,c('(0,6]','(6,12]','(12,18]'))
  rEnd$x = 0
  colnames(rEnd) = c('Date','End','x')
  rEnd_temp = aggregate(temp_clean$Date,by=list(Date=temp_clean$Date,End=temp_clean$End_int),length)
  rEnd_temp = rEnd_temp[!rEnd_temp$End=='(18,24]',]
  rEnd$x[match(as.character(interaction(rEnd_temp$Date,rEnd_temp$End)),
                 as.character(interaction(rEnd$Date,rEnd$End)))] = rEnd_temp$x
  rN = aggregate(temp_clean$Date,by=list(Date=temp_clean$Date),length)[,2]
  rEnd1 = rEnd$x[rEnd$End=='(0,6]']/rN
  rEnd2 = rEnd$x[rEnd$End=='(6,12]']/rN
  rEnd3 = rEnd$x[rEnd$End=='(12,18]']/rN
  
  panel = data.frame(y=temp_y,n=temp_n,date=dates,month=months,garage=garage,timecum=timecum,datecum=datecum,wd=wd,
                     rStart1=rStart1,rStart2=rStart2,rStart3=rStart3,
                     rEnd1=rEnd1,rEnd2=rEnd2,rEnd3=rEnd3)
  tpanel = apply(as.matrix(panel$date),1,FUN=tbmatch,udates=udates,tlist=tlist)
  panel$tint = as.factor(tpanel)
  lastpds = apply(as.matrix(c(1:nrow(panel))),1,FUN=lastpayday,dates=panel$date,payday=payday)
  panel$pdcum = as.numeric(as.Date(panel$date,origin='1970-01-01')) - lastpds
  
  # weather_temp = weather[match(dates,weather$date),]
  # panel = cbind(panel,weather_temp[,2:ncol(weather_temp)])
  
  panel.list[[k]] = panel
}

# train.list = list()
# test.list = list()
# for (k in 1:length(garage.list)){
#   panel = panel.list[[k]]
#   train = panel[1:630,]
#   test = panel[631:727,]
#   train.list[[k]] = train
#   test.list[[k]] = test
# }
# train = rbindlist(train.list)
# test = rbindlist(test.list)
# 
# m = glm(y~offset(log(n)) + garage*(
#       bs(datecum,knots=c(25,70,105,130,195,220,250,280,330)) +
#       timecum + wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + month)
#        ,data=train,family=poisson(link='log'))
# test$yr = predict(m,newdata=test,type='response')
# 
# garage = 'NIC'
# par(mfrow=c(1,2))
# plot(train$y[train$garage==garage]~dates[1:630],type='l',main='Train 2017-03-04 to 2018-11-31',
#      ylab = 'Counts.',xlab=garage)lookeval(eval,garage,c('2018-06-01','2019-03-09'),show32=F)
# lines(y=m$fitted.values[train$garage==garage],x=dates[1:630],col='red')
# plot(test$y[test$garage==garage]~dates[631:727],type='l',main='Forecast 2018-12-01 to 2019-03-09',
#      ylab ='Counts.',xlab=garage,lwd=2)
# lines(y=test$yr[test$garage==garage],x=dates[631:727],col='red',lwd=1)
# lines(y=0.32*test$n[test$garage==garage],x=dates[631:727],col='blue',lty=2)
# upw = qpois(0.95,test$yr)
# lines(y=upw[test$garage==garage],
#       x=dates[631:727],col='red',lty=2,lwd=1)
# legend('topleft',legend=c('Observation','32% Strategey','95 Conf Int.','Predicted Mean'),lty=c(1,2,2,1),
#        col=c('black','blue','red','red'))

# CV
pickdates = as.Date(c("2017-03-04","2017-06-17","2017-08-19","2017-12-02","2018-03-17",
                      "2018-06-09","2018-08-18","2018-12-01","2019-03-09"))
date.ind = list()
for (i in 1:length(pickdates)){
  left = pickdates[i]
  right= pickdates[i+1]
  date.vec = dates[(dates>=left)&(dates<=right)]
  date.ind[[i]] = match(date.vec,dates)
}

eval.list = list()
for (i in 1:(length(pickdates)-1)){
  
  # 1 construct train and test
  train.list = list()
  test.list = list()
  train.ind = c()
  for (j in 1:length(pickdates)){
    if (i!=j){
      train.ind = c(train.ind,date.ind[[j]])
    }
  }
  test.ind = date.ind[[i]]
  
  for (k in 1:length(garage.list)){
    panel = panel.list[[k]]
    train = panel[train.ind,]
    test = panel[test.ind,]
    train.list[[k]] = train
    test.list[[k]] = test
  }
  train = rbindlist(train.list)
  test = rbindlist(test.list)
  
  # 2 fit and predict
  m = glm(cbind(y,n-y)~ garage + 
            wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint
          ,data=train,family='binomial')
  test$yr = predict(m,newdata=test,type='response')*test$n
  test$upw = qbinom(0.95,test$n,test$yr/test$n)

  m = glm(cbind(y,n-y)~ bs(datecum,knots=c(25,70,95,130,195,215,230,250,280,330)) + garage + 
      wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint
    ,data=train,family='binomial')
  test$yrs = predict(m,newdata=test,type='response')*test$n
  
  # 2.5 use other models
  X.train = model.matrix(~  garage + 
                           wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                         train)
  Y.train = as.matrix(cbind(train$y,train$n-train$y))
  X.test = model.matrix(~  garage + 
                          wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                        test)
  
  m.lasso = cv.glmnet(X.train,Y.train,family='binomial',alpha=1)
  test$ylasso = (1-predict(m.lasso,newx=X.test,s='lambda.min',type='response'))*test$n
  
  m.ridge = cv.glmnet(X.train,Y.train,family='binomial',alpha=0)
  test$yridge = (1-predict(m.ridge,newx=X.test,s='lambda.min',type='response'))*test$n
  
  m.net = cv.glmnet(X.train,Y.train,family='binomial',alpha=0.5)
  test$ynet = (1-predict(m.net,newx=X.test,s='lambda.min',type='response'))*test$n

  X.train = model.matrix(~ bs(datecum,knots=c(25,70,95,130,195,215,230,250,280,330)) + garage +
                     wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                   train)
  Y.train = as.matrix(cbind(train$y,train$n-train$y))
  X.test = model.matrix(~ bs(datecum,knots=c(25,70,95,130,195,215,230,250,280,330)) + garage +
                           wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                         test)

  m.lasso = cv.glmnet(X.train,Y.train,family='binomial',alpha=1)
  test$ylassos = (1-predict(m.lasso,newx=X.test,s='lambda.min',type='response'))*test$n

  m.ridge = cv.glmnet(X.train,Y.train,family='binomial',alpha=0)
  test$yridges = (1-predict(m.ridge,newx=X.test,s='lambda.min',type='response'))*test$n

  m.net = cv.glmnet(X.train,Y.train,family='binomial',alpha=0.5)
  test$ynets = (1-predict(m.net,newx=X.test,s='lambda.min',type='response'))*test$n

  m.rpart = rpart(cbind(n,y)~ datecum + garage +
                    wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                  data=train,method='poisson')
  test$yrpartp = predict(m.rpart,newdata=test,type='vector')*test$n

  m.rpart = rpart(y~ n + datecum + garage +
                    wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                  data=train,method='anova')
  test$yrpartr = predict(m.rpart,newdata=test,type='vector')

  m.rpart = rpart(cbind(n,y)~ bs(datecum,knots=c(25,70,95,130,195,215,230,250,280,330)) + garage +
                    wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                  data=train,method='poisson')
  test$yrpartps = predict(m.rpart,newdata=test,type='vector')*test$n

  m.rpart = rpart(y~ n + bs(datecum,knots=c(25,70,95,130,195,215,230,250,280,330)) + garage +
                    wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                  data=train,method='anova')
  test$yrpartrs = predict(m.rpart,newdata=test,type='vector')

  m.rf = randomForest(y~ n + datecum + garage +
                        wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
                      data=train)
  test$yrf = predict(m.rf,newdata=test,type='response')

  m.svm = svm(y~ n + datecum + garage +
                wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
              data=train)
  test$ysvm = predict(m.svm,newdata=test)

  m.svm = svm(y~ n + bs(datecum,knots=c(25,70,95,130,195,215,230,250,280,330)) + garage +
                wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint,
              data=train)
  test$ysvms = predict(m.svm,newdata=test)
  
  # 3 combine
  eval.list[[i]] = test
  print(i)
}

eval = rbindlist(eval.list)

model.name = c('GLM','GLMlasso','GLMridge','GLMelasticNet'
               ,'GLM + Spline','GLMlasso + Spline','GLMridge + Spline','GLMelasticNet + Spline'
               ,'PoissonTree','RegTree','PoissonTree + Spline','RegTree + Spline', 'RandomForest'
               ,'SVM', 'SVM + Spline')
error.matrix = matrix(nrow=nrow(eval),ncol=length(model.name))
error.matrix[,1] = (eval$y - eval$yr)^2
error.matrix[,2] = (eval$y - eval$ylasso)^2
error.matrix[,3] = (eval$y - eval$yridge)^2
error.matrix[,4] = (eval$y - eval$ynet)^2
error.matrix[,5] = (eval$y - eval$yrs)^2
error.matrix[,6] = (eval$y - eval$ylassos)^2
error.matrix[,7] = (eval$y - eval$yridges)^2
error.matrix[,8] = (eval$y - eval$ynets)^2
error.matrix[,9] = (eval$y - eval$yrpartp)^2
error.matrix[,10] = (eval$y - eval$yrpartr)^2
error.matrix[,11] = (eval$y - eval$yrpartps)^2
error.matrix[,12] = (eval$y - eval$yrpartrs)^2
error.matrix[,13] = (eval$y - eval$yrf)^2
error.matrix[,14] = (eval$y - eval$ysvm)^2
error.matrix[,15] = (eval$y - eval$ysvms)^2
error.panel = matrix(nrow=3,ncol=length(model.name))
colnames(error.panel) = model.name
rownames(error.panel) = c('Mean','SE','Max')
error.panel[1,] = colMeans(error.matrix)
error.panel[2,] = sqrt(colMeans((error.matrix - error.panel[1,])^2)/nrow(error.matrix))
for (i in 1:length(model.name)){
  error.panel[3,i] = max(error.matrix[,i])
}
error.panel = round(error.panel,5)

garage = 'NIC'
lookeval(eval,garage,c('2017-01-01','2019-12-09'),pred='yr',show32=F)

# Fit the model
panel = rbindlist(panel.list)
# m = glm(cbind(y,n-y)~ garage*(
#   bs(datecum,knots=c(25,70,105,130,195,220,250,280,330)) +
#     timecum + wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + month )
#   ,data=panel,family='binomial')
m = glm(cbind(y,n-y)~ garage + 
          wd + rStart1 + rStart2 + rStart3 + rEnd1 + rEnd2 + rEnd3 + pdcum + timecum + month + tint
  ,data=panel,family='binomial')
write.csv(summary(m)$coefficients,file='glmout.csv')

############################################
# Poisson Binomial Model
dates = unique(clean$Date)
Y.panel = aggregate(clean$Uncover,by=list(cat1=clean$Date,cat2=clean$Garage),sum)
referX = function(i,Y.panel,clean){
  date = Y.panel[i,1]
  garage = Y.panel[i,2]
  subclean = clean[(clean$Date==date)&(clean$Garage==garage),]
  
  # X = model.matrix(~bs(datecum,knots=c(25,70,95,130,195,215,230,250,280,330)) + Garage +
  #      wd + X1p.2p + Duty.type + Start_int * Duration + From_M + To_M + timecum + pdcum + months + tint
  #     ,data=subclean)
  X = model.matrix(~ Garage +
                     wd + X1p.2p + Duty.type + Start_int * Duration + From_M + To_M + timecum + pdcum + months + tint
                   ,data=subclean)
  return(X)
}
Y.list = Y.panel[,3]
X.list = apply(as.matrix(c(1:nrow(Y.panel))),1,FUN=referX,Y.panel=Y.panel,clean)
Y.panel$mu = 0
Y.panel$upw = 0

# CV
for (pick.ind in 1:8){
  test.ind = which((as.Date(Y.panel$cat1,origin='1970-01-01')>=pickdates[pick.ind])&
                     (as.Date(Y.panel$cat1,origin='1970-01-01')<=pickdates[pick.ind+1]))
  train.ind = c(1:nrow(Y.panel))[-test.ind]
  train.Y.list = Y.list[train.ind]
  test.Y.list = Y.list[test.ind]
  train.X.list = X.list[train.ind]
  test.X.list = X.list[test.ind]
  
  theta = glmPBD(train.Y.list,train.X.list,epsi=1e-07,diffup=1,L2=1e06,batch=T)
  fit = predictPBD(test.X.list,theta)
  Y.panel[test.ind,'mu'] = fit$mu 
  Y.panel[test.ind,'upw']= fit$upw
  print(pick.ind)
}

eval.ind = which((as.Date(Y.panel$cat1,origin='1970-01-01')>=pickdates[1])&
                   (as.Date(Y.panel$cat1,origin='1970-01-01')<=pickdates[9]))
eval.Y.panel = Y.panel[eval.ind,]
dif = (eval.Y.panel$x - eval.Y.panel$mu)^2
c(mean(dif),max(dif),sqrt(var(dif)/length(dif)))

garage.list = c('NIC','FTH','SOU','MJR','EMG')
for (i in 1:5){
  garage = garage.list[i]
  eval.Y.panel = Y.panel[eval.ind,]
  eval.Y.panel = eval.Y.panel[eval.Y.panel$cat2==garage,]
  dif = abs(eval.Y.panel$x - eval.Y.panel$mu)
  print(garage)
  print(c(min(dif),mean(dif),max(dif)))
}

lookPBDeval(Y.panel,garage='NIC',crit=c('2017-03-01','2019-03-09'))
abline(v=seq.Date(from=as.Date('2017-01-01',origin='1970-01-01'),
                  to=as.Date('2019-12-01',origin='1970-01-01'),by='month'),col='orange')
abline(v=payday,col='green')






