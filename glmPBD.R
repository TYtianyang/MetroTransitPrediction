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
  return(log(phi)+((Y-mu)^2)/(phi^2))
}

gradlogL = function(theta,X,Y){
  p = xtop(X,theta)
  mu = ptomu(p)
  phi = ptophi(p)
  grad1 = (Y-mu)*(colSums(p*(1-p)*X))/(phi^2)
  grad2 = 0.5*(((Y-mu)^2)/(phi^4)-1/phi^2)*(colSums((2*p-1)*(1-p)*p*X))
  return(as.matrix(grad1-grad2))
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
  return(theta)
}

predictPBD = function(X.list,theta){
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
  upw = qnorm(0.95,mean=mu_vec,sd=phi_vec)
  return(list(mu=mu_vec,upw=upw))
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






