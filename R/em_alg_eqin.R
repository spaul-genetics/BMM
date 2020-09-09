em_alg_eqin<-function(data,max_it,alpha=0.5,mu=0,beta=0,epsilon = 1e-7){
  N<-data$Y.length
  Y<-data$Y.sum
  G<-data$G
  mu1<-mu
  mu2<-mu

  lik<-c() # Initiate a vector to keep log-likelihood
  lak<-0 # For AT stopping criterion

  sigmoid<-function(N,Y,X){
    choose(N,Y)*exp(X*Y)/(1+exp(X))^N
  }

  cost<-function(theta){
    sum((1-ez2)*log(sigmoid(N,Y,theta[1])))+sum(ez2*log(sigmoid(N,Y,theta[1]+theta[2]*G)))
  }
  for(i in 1:max_it){
    com1<-(1-alpha)*choose(N,Y)*exp(mu1*Y)/(1+exp(mu1))^N
    com2<-alpha*choose(N,Y)*exp((mu2+beta*G)*Y)/((1+exp(mu2+beta*G))^N)
    ez2 <- com2/(com1+com2)
    alpha <- mean(ez2)
    theta_optim <- optim(par=c(mu1,beta),fn=cost,control = list(fnscale = -1))

    mu1<-theta_optim$par[1]
    mu2<-theta_optim$par[1]
    beta<-theta_optim$par[2]
    likelihood_com1<-(1-alpha)*choose(N,Y)*exp(mu1*Y)/(1+exp(mu1))^N
    likelihood_com2<-alpha*choose(N,Y)*exp((mu2+beta*G)*Y)/((1+exp(mu2+beta*G))^N)
    curr_loglik<-sum(log(likelihood_com1+likelihood_com2))
    lik[i]<-curr_loglik

    incom_lik<-function(theta){
      mu0<-theta[1]
      beta0<-theta[2]
      -1*sum(log((1-alpha)*exp(mu0*Y)/(1+exp(mu0))^N+alpha*exp((mu0+beta0*G)*Y)/(1+exp(mu0+beta0*G))^N))
    }
    I<-numDeriv::hessian(incom_lik,c(mu1,beta))
    cov_matrix<-solve(I)
    var<-cov_matrix[2,2]
    if(i>2){
      if(lik[i-1]==lik[i-2]) break
      ak<-(lik[i]-lik[i-1])/(lik[i-1]-lik[i-2])
      if(ak==1) break
      lakp1<-lik[i-1]+(lik[i]-lik[i-1])/(1-ak)
      if(abs(lakp1-lak)<epsilon) break
      lak<-lakp1
    }
  }
  return(list(it = i,alpha = alpha,mu1=mu1,mu2=mu2,beta=beta,curr_loglik=curr_loglik,var=var))
}
