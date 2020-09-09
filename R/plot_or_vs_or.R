plot_OR_vs_OR<-function(dat,mix,pp){
  dat$OR_sim<-exp(dat$true_beta)
  dat$OR_mix<-exp(dat$beta)
  dat$OR_logit<-exp(dat$non_mix_estimate)
  dat<-dat[dat$mix_param==mix & dat$maf == pp,]
  box_data<-dat[,c('OR_sim','OR_mix','OR_logit')]
  names(box_data)<-c('OR','Mixture','Logistic')
  box_data$OR <- as.factor(round(box_data$OR,2))
  box_data_melt<-reshape2::melt(box_data,id.vars = 'OR')
  tb<-seq(-1,1,0.1)
  flag<-data.frame(x=as.factor(round(exp(tb),2)),y=exp(tb))
  p<-ggplot2::ggplot(data = box_data_melt, aes(x=OR, y=value, fill=variable))+theme_bw(base_size = 8) +
    geom_boxplot(outlier.shape = 21,outlier.stroke = 0.2,outlier.size = 0.03,lwd=0.08)+
    geom_line(data = flag, aes(x=x,y = y, group =1),inherit.aes = FALSE,size = 0.2)+
    theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size = 0.05, linetype = 'dotted',
                                      colour = "blue"),
    panel.grid.major.y = element_blank(),
    #plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(angle = 90),
    legend.position = 'bottom',legend.key.size = unit(0.5,"line"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-3, -3, -3, -3)
  )+
  guides(fill=guide_legend(title="Model"))+
  xlab('OR') + ylab('Estimated OR')+ylim(0.25,2.6)
  return(p)
}
