t_test_bias<-function(dat){
  mafs<-unique(dat$maf)
  betas<-unique(dat$true_beta)
  mixs<-unique(dat$mix_param)
  t_test_result_bias<-data.frame()
  for(m in mixs){
    for(p in mafs){
      for(b in betas){
        print(c(m,p,b))
        dat_subset<-dat[dat$mix_param==m & dat$maf==p & dat$true_beta==b,]
        print(nrow(dat_subset))
        t_test<-t.test(dat_subset$beta- b,dat_subset$non_mix_estimate-b,paired = T,alternative = "two.sided")
        t_test_result_bias<-rbind(t_test_result_bias,c(m,p,b,t_test$p.value))
      }
    }
  }
  names(t_test_result_bias)<-c('alpha','maf','beta','pvalue')
  return(t_test_result_bias)
}
