
lrts_qqplot<-function(data, p=NA, title='',size=1){
  if(is.na(p)){
    dat_null<-data[data$mix_param==0 | data$true_beta==0,]
  }else{
    dat_null<-data[data$maf==p,]
    dat_null<-dat_null[dat_null$mix_param==0 | dat_null$true_beta==0 ,]
  }
  ggplot(dat_null,aes(sample = lik_ratio_stat))+
    stat_qq(distribution = qchibarsq, dparams = list(df=2))+
    geom_abline(slope = 1,intercept = 0,color = 'red')+
    #stat_qq_line(distribution = qchibarsq, dparams = list(df=2),color = 'red')+
    #theme_bw(base_size = 30)+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5,color = 'red'))+
    theme_bw(base_size = size)
}
