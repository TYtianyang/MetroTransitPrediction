scolSums = function(X){
  return(colSums(X))
}
xtop = function(X,theta){
  return(as.vector(1/(1+exp(X%*%theta)^(-1))))
}

ptomu = function(p){
  return(sum(p))
}

ptophi = function(p){
  return(sqrt(sum(p*(1-p))))
}

logL = function(theta,X,Y){
  p = xtop(X,theta)
  mu = ptomu(p)
  phi = ptophi(p)
  return(-log(phi)-((Y-mu)^2)/(phi^2))
}

gradlogL = function(theta,X,Y){
  p = xtop(X,theta)
  mu = ptomu(p)
  phi = ptophi(p)
  grad1 = (Y-mu)*(scolSums(p*(1-p)*X))/(phi^2)
  grad2 = 0.5*(((Y-mu)^2)/(phi^4)-1/phi^2)*(scolSums((2*p-1)*(1-p)*p*X))
  return(as.matrix(grad1-grad2))
}

hesslogL = function(theta,X,Y){
  p = xtop(X,theta)
  mu = ptomu(p)
  phi = ptophi(p)
  
  comp1 = 0.5*(((Y-mu)^2)/(phi^4)-1/phi^2)
  comp2 = as.matrix((scolSums((1-2*p)*(1-p)*p*X)))
  comp3 = as.matrix((Y-mu)*(scolSums(p*(1-p)*X))/(phi^2))
  
  dcomp1 = as.matrix(0.5*(1/phi^4 - 2/phi^6*(Y-mu)^2)*(scolSums((1-2*p)*p*(1-p)*X)) - 1/phi^4*(Y-mu)*(scolSums(p*(1-p)*X)))
  dcomp2 = t(X)%*%((1-6*p+6*p^2)*p*(1-p)*X)
  dcomp3 = -1/phi^4*(Y-mu)*as.matrix(scolSums(p*(1-p)*X))%*%(scolSums((1-2*p)*p*(1-p)*X)) + 
    1/phi^2*( as.matrix(scolSums(p*(1-p)*X))%*%(-scolSums(p*(1-p)*X)) + (Y-mu)*(t(X)%*%((1-2*p)*p*(1-p)*X)) )
  
  hess = dcomp1 %*% t(comp2) + comp1*dcomp2 + dcomp3
  return(hess)
}

formatPBD = function(form,data,Y=T){ # Need to generalize
  form = as.formula(paste(c('~',as.character(form)[3]),collapse=''))
  data = data[order(data$Date,data$Garage,data$Time_part,data$Duty_cat),]
  group_panel = data.frame(date=as.character(data$Date),garage=data$Garage,time_part=data$Time_part,duty_cat=data$Duty_cat)
  group_fac = apply(group_panel,1,FUN=paste,collapse='_')
  X.list = split(data,group_fac)
  
  if (Y){
    Y.list = aggregate(data$Uncover,by=list(Gr=group_fac),FUN=sum)[,2]
  }else{
    Y.list = aggregate(rep(0,nrow(data)),by=list(Gr=group_fac),FUN=sum)[,2]
  }
  
  Y.panel = as.data.frame(t(as.matrix(as.data.frame(strsplit(unique(group_fac),split='_')))))
  rownames(Y.panel) = c(1:length(unique(group_fac)))
  colnames(Y.panel) = c('Date','Garage','Time_part','Duty_cat')
  Y.panel$x = Y.list
  Y.panel$n = aggregate(data$Date,by=list(group_fac=group_fac),FUN=length)[,2]
  
  revMM = function(i,X.list,form){
    return(model.matrix(form,X.list[[i]]))
  }
  X.list = apply(as.matrix(c(1:length(Y.list))),1,FUN=revMM,X.list=X.list,form=form)  

  return(list(Y.list=Y.list,X.list=X.list,Y.panel=Y.panel))
}

glmPBDoptim = function(Y.list,X.list,verbose=T){
  
  theta = rep(0,ncol(X.list[[1]]))
  fn = function(theta,Y.list,X.list){
    logLvalue = 0
    for (i in 1:length(Y.list)){
      Y = Y.list[i]
      X = X.list[[i]]
      logLvalue = logLvalue + logL(as.matrix(theta),X,Y)
    }
    return(-logLvalue)
  }

  gr = function(theta,Y.list,X.list){
    gradlogLvalue = rep(0,length(theta))
    for (i in 1:length(Y.list)){
      Y = Y.list[i]
      X = X.list[[i]]
      gradlogLvalue = gradlogLvalue + as.vector(gradlogL(as.matrix(theta),X,Y))
    }
    return(-gradlogLvalue)
  }

  result = optim(par=theta,fn=fn,gr=gr,method='BFGS',Y.list=Y.list,X.list=X.list)
  theta = result$par
  
  if (verbose){
    hess = matrix(0,length(theta),length(theta))
    for (i in 1:length(Y.list)){
      subhess = hesslogL(theta,X.list[[i]],Y.list[i])
      hess = hess + subhess
    }
    hess = -hess/length(Y.list)
    invhess = ginv(hess)
    se = sqrt(diag(invhess)/length(Y.list))
    zscore = theta/se
    pvalue = 2*(1-pnorm(abs(zscore)))
    panel = data.frame(Coef = round(theta,4),SE = round(se,4), Z_value = round(zscore,4), p_value = round(pvalue,4))
    rownames(panel) = colnames(X.list[[1]])
    print(panel)}else{panel='None'}
  
  return(list(theta=theta,panel=panel))
}


glmPBD = function(Y.list,X.list,epsi=1e-06,diffup = 10,L2=1e06,batch=T){
  
  singleUpdate = function(i,Y.list,X.list,theta){
    Y=Y.list[i]
    X=X.list[[i]]
    return(list(l=logL(theta,X,Y),g=gradlogL(theta,X,Y)))
  }
  
  theta = as.matrix(rep(0,ncol(X.list[[1]])))
  diff = diffup*10
  l.vec = c()
  t = 1
  if (batch){
    while(diff > diffup){
      for (X.ind in 1:length(Y.list)){
        output = singleUpdate(X.ind,Y.list,X.list,theta)
        g = output$g
        theta = theta + epsi*g/max(1,sqrt(sum(g^2)/L2))
      }
      update.list = apply(as.matrix(c(1:length(Y.list))),1,FUN=singleUpdate,theta=theta,Y.list=Y.list,X.list=X.list)
      l=0
      for (i in 1:length(update.list)){
        l = l + update.list[[i]]$l
      }
      l.vec[t] = l
      if (t!=1){
        diff = abs(l.vec[t] - l.vec[t-1])
      }
      if (mod(t,10)==1){
        plot(l.vec,type='l',main='Negative LogL',ylim=c(l.vec[1]/100,l.vec[1]))
      }
      t = t+1
    }
  }else{
    while(diff > diffup){
      update.list = apply(as.matrix(c(1:length(Y.list))),1,FUN=singleUpdate,theta=theta,Y.list=Y.list,X.list=X.list)
      l=0
      g=as.matrix(rep(0,length(theta)))
      for (i in 1:length(update.list)){
        l = l + update.list[[i]]$l
        g = g + update.list[[i]]$g
      }
      theta = theta + epsi*g/max(1,sqrt(sum(g^2)/L2))
      l.vec[t] = l
      if (t!=1){
        diff = abs(l.vec[t] - l.vec[t-1])
      }
      if (mod(t,10)==1){
        plot(l.vec,type='l',main='Negative LogL',ylim=c(l.vec[1]/100,l.vec[1]))
      }
      t = t+1
    }
  }
  plot(l.vec,type='l',main='Negative LogL')
  
  hess = matrix(0,length(theta),length(theta))
  for (i in 1:length(Y.list)){
    hess = hess + hesslogL(theta,X.list[[i]],Y.list[i])
  }
  print(theta)
  print(hess)
  se = sqrt(diag(ginv(hess)))
  zscore = theta/se
  pvalue = 2*(1-pnorm(abs(zscore)))
  panel = data.frame(Coef = theta,SE = se, Z_value = zscore, p_value = pvalue)
  rownames(panel) = names(theta)
  print(panel)

  return(list(theta=theta,report=panel))
}

predictPBD = function(X.list,model,upper.tail=0.95){
  theta = model$theta
  singlePredict = function(i,X.list,theta){
    X = X.list[[i]]
    p = xtop(X,theta)
    mu = ptomu(p)
    phi = ptophi(p)
    return(list(mu=mu,phi=phi))
  }
  fit = apply(as.matrix(c(1:length(X.list))),1,FUN=singlePredict,X.list=X.list,theta=theta)
  mu_vec = c()
  phi_vec = c()
  for (i in 1:length(fit)){
    mu_vec[i] = fit[[i]]$mu
    phi_vec[i] = fit[[i]]$phi
  }
  upw = qnorm(upper.tail,mean=mu_vec,sd=phi_vec)
  return(list(mu=mu_vec,upw=upw,phi=phi_vec,theta=theta))
}

evalPBD = function(Y.list,pred){
  mu = pred$mu
  phi = pred$phi
  theta = pred$theta
  aic = sum(log(phi)+((Y.list-mu)^2)/(phi^2))+length(theta)
  bic = 2*sum(log(phi)+((Y.list-mu)^2)/(phi^2))+log(length(Y.list))*length(theta)
  mse = mean((Y.list-mu)^2)
  return(list(aic=aic,bic=bic,mse=mse))
}

# set.seed(2019)
# X.list = list()
# Y.list = c()
# for (i in 1:1000){
#   X1 = rnorm(1000)
#   X2 = rnorm(1000)*1.5+1
#   X = matrix(0,ncol=2,nrow=1000)
#   X[,1] = X1
#   X[,2] = X2
#   theta0 = as.matrix(c(0.3,0.8))
#   p = xtop(X,theta0)
#   Y = sum(rbinom(1000,1,p))
#   Y.list[i] = Y
#   X.list[[i]] = X
# }






