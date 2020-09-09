bmm_eqin<-function(data,alpha=0.5,mu=0,beta=0,max_it=50,epsilon = 1e-7,rep_em = 7){
  gr_dat<-doBy::summaryBy(Y~Cluster + G,data = data,FUN = c(sum,length))

  N<-gr_dat$Y.length
  Y<-gr_dat$Y.sum
  G<-gr_dat$G

  em1<-em_alg_eqin(data = gr_dat,max_it = 5000,alpha = 0.5,mu=mu,beta = beta,epsilon = 1e-10)
  em<-em1
  if(exp(abs(em1$beta))>rep_em){
    em2<-em_alg_eqin(data = gr_dat,max_it = 5000,alpha = 0.1,mu=mu,beta = beta,epsilon = 1e-10)
    if(em2$curr_loglik>em1$curr_loglik){
      em<-em2
      print('problem fixed')
    }
  }


  if(exp(abs(em$beta))>rep_em){
    em3<-em_alg_eqin(data = gr_dat,max_it = 5000,alpha = 0.9,mu=mu,beta = beta,epsilon = 1e-10)
    if(em3$curr_loglik>em$curr_loglik){
      em<-em3
      print('problem fixed')
    }
  }


  lik_logit<-logLik(glm(matrix(c(Y,N-Y),nc=2)~G,data=gr_dat,family = binomial()))
  non_mix_model<-glm(matrix(c(Y,N-Y),nc=2)~G,data=gr_dat,family = binomial())
  null_log_like<-logLik(glm(matrix(c(Y,N-Y),nc=2)~1,data = gr_dat,family = binomial()))
  lik_ratio_logit<-2*(lik_logit-null_log_like)
  neg_2_log_lik_ratio<- 2*(em$curr_loglik > null_log_like)*(em$curr_loglik - null_log_like)
  #print(c(summary(non_mix_model)$coefficients[2,c(1,4)],1-pchisq(neg_2_log_lik_ratio,1)))
  out<-c(as.integer(em$it),em$alpha,em$mu1,em$mu2,em$beta,em$curr_loglik,em$var,neg_2_log_lik_ratio,summary(non_mix_model)$coefficients[2,c(1,4
  )],lik_logit,lik_ratio_logit)
  names(out)<-c('Iterations','Mix_param','mu1','mu2','beta','likelihood','var_beta','lik_ratio_stat','non_mix_estimate','non_mix_pvalue','lik_logit
','lik_ratio_logit')
  return(out)
}

