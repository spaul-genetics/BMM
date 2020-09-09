lrts_density<-function(data, p=NA, title='',size=1){
  if(is.na(p)){
    dat_null<-data[data$mix_param==0 | data$true_beta==0,]
  }else{
    dat_null<-data[data$maf==p,]
    dat_null<-dat_null[dat_null$mix_param==0 | dat_null$true_beta==0 ,]
  }
  x<-seq(0,max(na.omit(dat_null$lik_ratio_stat)),0.01)
  #theme_set(theme_grey(base_size = 28))
  ggplot(dat_null,aes(x=lik_ratio_stat,colour='red'))+

    geom_density(trim=TRUE,show.legend =FALSE)+
    stat_density(aes(x=lik_ratio_stat),geom = 'line',trim = TRUE)+
    geom_line(data = data.frame(x=x),aes(x=x,y=0.5*dchisq(x,1)+0.5*dchisq(x,2),colour='blue'))+
    scale_colour_manual(values = c('red' = 'red','blue' = 'blue'),name = '',
                        labels = expression(paste('1/2',chi[1]^2,'+1/2',chi[2]^2),'LRTS'))+
    labs(x='Likelihood Ratio Statistic',y='Density',title = title)+
    ylim(0,1)+theme_bw(base_size = size)
}
