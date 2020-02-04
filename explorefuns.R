lookseries = function(dta,id,crit){
  crit = as.Date(crit,origin="1970-01-01")
  temp = dta[(dta$Date>=crit[1])&(dta$Date<=crit[2])&(dta$Work_id==id),]
  temp_n = aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,2]
  temp_n0 = aggregate(temp$uncover,list(Category=temp$Date),FUN=sum)[,2]
  temp_p = temp_n0/temp_n
  temp_data = data.frame(date=seq(from=crit[1],to=crit[2],by='day'),p=NA)
  temp_data$p[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_p
  plot(p~date,data=temp_data,type='l',main=id,xlab='Time',ylab='Prob. Uncover')
}

lookfit = function(dta,id,crit){
  crit = as.Date(crit,origin="1970-01-01")
  temp = dta[(dta$Date>=crit[1])&(dta$Date<=crit[2])&(dta$Work_id==id),]
  temp_n0 = aggregate(temp$uncover,list(Category=temp$Date),FUN=sum)[,2]
  temp_n = aggregate(temp$uncover,list(Category=temp$Date),FUN=length)[,2]
  temp_fit = aggregate(temp$fit,list(Category=temp$Date),FUN=sum)[,2]
  temp_data = data.frame(date=seq(from=crit[1],to=crit[2],by='day'),n0=NA,n=NA,fit=NA)
  temp_data$n0[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_n0
  temp_data$n[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_n
  temp_data$fit[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_fit
  plot(n~date,data=temp_data,type='l',lty=2,main=id,xlab='Time',ylab='# Uncover',ylim=c(0,max(temp_data$n,na.rm=T)+2))
  lines(fit~date,data=temp_data,col='red')
  lines(n0~date,data=temp_data,col='black')
}

lookfitp = function(dta,id,crit){
  crit = as.Date(crit,origin="1970-01-01")
  temp = dta[(dta$Date>=crit[1])&(dta$Date<=crit[2])&(dta$Work_id==id),]
  temp_n = aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,2]
  temp_n0 = aggregate(temp$uncover,list(Category=temp$Date),FUN=sum)[,2]
  temp_p = temp_n0/temp_n
  temp_fitp = aggregate(temp$fit,list(Category=temp$Date),FUN=mean)[,2]
  temp_data = data.frame(date=seq(from=crit[1],to=crit[2],by='day'),p=NA,fitp=NA)
  temp_data$p[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_p
  temp_data$fitp[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_fitp
  plot(p~date,data=temp_data,type='l',main=id,xlab='Time',ylab='Prob. Uncover')
  lines(fitp~date,data=temp_data,col='red')
}

lookroc = function(dta){
  pred = prediction(dta$fit, dta$Uncover)
  perf = performance(pred,"tpr","fpr")
  plot(perf,colorize=TRUE)
  roc(dta,Uncover,fit)
}

lookNS = function(dta,garage,op,crit,p=FALSE,fit=FALSE){
  pickdates = as.Date(c("2017-06-17","2017-08-19","2017-12-02","2018-03-17",
                        "2018-06-09","2018-08-18","2018-12-01","2019-03-09"))
  crit = as.Date(crit,origin="1970-01-01")
  pickdates = pickdates[(pickdates>crit[1])&(pickdates<crit[2])]
  temp = dta[(dta$Date>=crit[1])&(dta$Date<=crit[2])&(dta$Garage==garage)&(dta$Operator_type%in%op),]
  temp_n = aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,2]
  temp_n0 = aggregate(temp$uncover,list(Category=temp$Date),FUN=sum)[,2]
  temp_p = temp_n0/temp_n
  if (fit){
    temp_fit = aggregate(temp$fit,list(Category=temp$Date),FUN=sum)[,2]
    temp_fitp = aggregate(temp$fit,list(Category=temp$Date),FUN=mean)[,2]
  }

  temp_data = data.frame(date=seq(from=crit[1],to=crit[2],by='day'),n=NA,n0=NA,fit=NA,p=NA,fitp=NA)
  temp_data$n[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_n
  temp_data$n0[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_n0
  temp_data$p[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_p
  if (fit){
    temp_data$fit[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_fit
    temp_data$fitp[match(aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,1],temp_data$date)] = temp_fitp
  }
  
  if (p){
    plot(p~date,data=temp_data,col='black',type='l',xlab=paste(crit,collapse='   to   '),ylab='Prob.',ylim=c(0,max(p)),
         main=paste(c(garage,'::',op),collapse=' '))
    abline(v=pickdates,col='blue',lty=2)
    if (fit){
      lines(fitp~date,data=temp_data,col='red')
    }
  }else{
    plot(n~date,data=temp_data,col='black',type='l',xlab=paste(crit,collapse='   to   '),ylab='Counts.',ylim=c(0,max(n)),
         main=paste(c(garage,'::',op),collapse=' '))
    abline(v=pickdates,col='blue',lty=2)
    lines(n0~date,data=temp_data,col='black',lty=2)
    if (fit){
      lines(fit~date,data=temp_data,col='red')
    }
  }
  return(temp_data)
}

lookNSclean = function(dta,garage,crit,p=FALSE,fit=FALSE){
  pickdates = as.Date(c("2017-06-17","2017-08-19","2017-12-02","2018-03-17",
                        "2018-06-09","2018-08-18","2018-12-01","2019-03-09"))
  crit = as.Date(crit,origin="1970-01-01")
  pickdates = pickdates[(pickdates>crit[1])&(pickdates<crit[2])]
  temp = dta[(as.Date(dta$Date,origin='1970-01-01')>=crit[1])&(as.Date(dta$Date,origin='1970-01-01')<=crit[2])&(dta$Garage==garage),]
  temp_n = aggregate(temp$Date,list(Category=temp$Date),FUN=length)[,2]
  temp_n0 = aggregate(temp$Uncover,list(Category=temp$Date),FUN=sum)[,2]
  temp_p = temp_n0/temp_n
  if (fit){
    temp_fit = aggregate(temp$fit,list(Category=temp$Date),FUN=sum)[,2]
    temp_fitp = aggregate(temp$fit,list(Category=temp$Date),FUN=mean)[,2]
  }
  
  temp_data = data.frame(date=unique(as.Date(temp$Date,origin='1970-01-01')),n=temp_n,n0=temp_n0,fit=NA,p=temp_p,fitp=NA)
  if (fit){
    temp_data$fit = temp_fit
    temp_data$fitp = temp_fitp
  }
  
  if (p){
    plot(p~date,data=temp_data,col='black',type='l',xlab=paste(crit,collapse='   to   '),ylab='Prob.',ylim=c(0,max(p)),
         main=garage)
    abline(v=pickdates,col='blue',lty=2)
    if (fit){
      lines(fitp~date,data=temp_data,col='red')
    }
  }else{
    plot(n~date,data=temp_data,col='black',type='l',xlab=paste(crit,collapse='   to   '),ylab='Counts.',ylim=c(0,max(n)),
         main=garage)
    abline(v=pickdates,col='blue',lty=2)
    lines(n0~date,data=temp_data,col='black',lty=2)
    if (fit){
      lines(fit~date,data=temp_data,col='red')
    }
  }
  return(temp_data)
}

lookAssignDuty = function(season,raw_roster_list){
  raw_roster = raw_roster_list[[season]]
  panel = aggregate(raw_roster$Id,by=list(Roster=raw_roster$Roster,Day=raw_roster$Day),length)
  colnames(panel) = c('Type','Day','Counts')
  panel = panel[panel$Day%in%c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),]
  panel$Day = factor(panel$Day,c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))
  panel$Type = factor(panel$Type,c('FT8','FT9','FT10','PTD','PTE','PTROS'))
  p1 = ggplot(data=panel,aes(x=Type,y=Counts,fill=Day)) + 
    geom_bar(stat="identity", color="black", position=position_dodge()) + 
    ggtitle(paste(c('Assigned Duties ',season),collapse=''))
  
  panel = aggregate(raw_roster$Id,by=list(Roster=raw_roster$Roster,`1p/2p`=raw_roster$Type),length)
  colnames(panel) = c('Type','1p/2p','Counts')
  panel = panel[panel$Type%in%c('FT8','FT9','FT10'),]
  panel$Type = factor(panel$Type,c('FT8','FT9','FT10'))
  p2 = ggplot(data=panel,aes(x=Type,y=Counts,fill=`1p/2p`)) + 
    geom_bar(stat="identity", color="black") + 
    ggtitle(paste(c('Assigned Duties ',season),collapse=''))
  
  panel = aggregate(raw_roster$Id,by=list(Roster=raw_roster$Roster,Garage=raw_roster$Garage),length)
  colnames(panel) = c('Type','Garage','Counts')
  panel = panel[panel$Type%in%c('FT8','FT9','FT10'),]
  panel$Type = factor(panel$Type,c('FT8','FT9','FT10'))
  p3 = ggplot(data=panel,aes(x=Garage,y=Counts,fill=Type)) + 
    geom_bar(stat="identity", color="black",position=position_dodge()) + 
    ggtitle(paste(c('Assigned Duties ',season),collapse=''))
  
  pie = ggplot(panel, aes(x="", y=Counts, fill=Type)) + geom_bar(stat="identity", width=1) + 
    coord_polar("y", start=0) +
    labs(x = NULL, y = NULL, fill = NULL, title = paste(c('Assigned Duties ',season),collapse='')) + 
    theme_classic() + 
    theme(axis.line = element_blank(),axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "#666666"))+
    facet_wrap(~Garage)
                            
  
  return(list(p1=p1,p2=p2))
}

lookeval = function(eval,ga,crit,pred,show32=F){
  pickdates = as.Date(c("2017-03-04","2017-06-17","2017-08-19","2017-12-02","2018-03-17",
                        "2018-06-09","2018-08-18","2018-12-01","2019-03-09"))
  crit = as.Date(crit,origin='1970-01-01')
  temp_eval = eval[(eval$date<=crit[2])&(eval$date>=crit[1])&(eval$garage==ga),]
  plot(y~date,data=temp_eval,type='l',main=paste('GLM Transf. Data CV:',ga))
  lines(y=temp_eval[[pred]],x=temp_eval$date,col='red')
  lines(upw~date,data=temp_eval,col='red',lty=2)
  abline(v=pickdates[(pickdates<=crit[2])&(pickdates>=crit[1])],col='blue',lty=2)
  if (show32){
    lines(y=temp_eval$n*0.32,x=temp_eval$date,col='green',lty=2)
    legend('topright',legend=c('Data','Predicted Mean','0.95 Conf Int','32% Strategy'),
           col=c('black','red','red','green'),lty=c(1,1,2,2))
  }else{
    legend('topright',legend=c('Data','Predicted Mean','.95 Conf Int'),
           col=c('black','red','red'),lty=c(1,1,2))
  }
}

lookPBDeval = function(Y.panel,garage,crit){
  pickdates = as.Date(c("2017-03-04","2017-06-17","2017-08-19","2017-12-02","2018-03-17",
                        "2018-06-09","2018-08-18","2018-12-01","2019-03-09"))
  crit = as.Date(crit,origin='1970-01-01')
  test.Y.panel = Y.panel[(as.Date(Y.panel$cat1,origin='1970-01-01')<=crit[2])&
                           (as.Date(Y.panel$cat1,origin='1970-01-01')>=crit[1])&(Y.panel$cat2==garage),]
  plot(y=test.Y.panel$x,x=as.Date(test.Y.panel$cat1,origin='1970-01-01'),type='l',main=paste('PBD CV:',garage),ylab='# Uncovers',xlab='Time')
  lines(y=test.Y.panel$mu,x=as.Date(test.Y.panel$cat1,origin='1970-01-01'),col='red')
  lines(y=test.Y.panel$upw,x=as.Date(test.Y.panel$cat1,origin='1970-01-01'),col='red',lty=2)
  abline(v=pickdates[(pickdates<=crit[2])&(pickdates>=crit[1])],col='blue',lty=2)
  legend('topright',legend=c('Data','Predicted Mean','.95 Conf Int'),
         col=c('black','red','red'),lty=c(1,1,2))
}
