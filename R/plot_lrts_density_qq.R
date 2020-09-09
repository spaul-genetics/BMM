plot_lrts_density_qq<-function(data, maf,size=20,text.size=20){
  a1<-lrts_density(data = data,p = maf[1],title = '',size = size)+theme(axis.title = element_blank())+theme(plot.margin = margin(t=-30,r = 0,b=5,l=5,unit = 'pt'))
  a2<-lrts_density(data = data,p = maf[2],title = '',size = size)+theme(axis.title = element_blank(),axis.text.y = element_blank())+theme(plot.margin = margin(t=-30,r = 0,b=5,l=0,unit = 'pt'))
  a3<-lrts_density(data = data,p = maf[3],title = '',size = size)+theme(axis.title = element_blank(),axis.text.y = element_blank())+theme(plot.margin = margin(t=-30,r = 15,b=5,l=0,unit = 'pt'))
  b1<-lrts_qqplot(data = data,p=maf[1],title = paste('maf = ',maf[1],sep = ''),size = size)+theme(axis.title = element_blank())+theme(plot.margin = margin(t=5,r = 0,b=10,l=5,unit = 'pt'))
  b2<-lrts_qqplot(data = data,p=maf[2],title = paste('maf = ',maf[2],sep = ''),size = size)+theme(axis.title = element_blank(),axis.text.y = element_blank())+theme(plot.margin = margin(t=5,r = 0,b=5,l=0,unit = 'pt'))
  b3<-lrts_qqplot(data = data,p=maf[3],title = paste('maf = ',maf[3],sep = ''),size = size)+theme(axis.title = element_blank(),axis.text.y = element_blank())+theme(plot.margin = margin(t=5,r = 5,b=5,l=0,unit = 'pt'))


  g1<-ggarrange(a1,a2,a3,ncol = 3,nrow = 1,common.legend = T,legend = 'top',widths = c(1.2,1,1))
  g1<-annotate_figure(g1,
                      bottom = text_grob("Likelihood Ratio Statistic", color = "black",size = text.size),
                      left = text_grob('Density',color = 'black',rot = 90,size = text.size)
  )
  g2<-ggarrange(b1,b2,b3,ncol = 3,nrow = 1,widths = c(1.1,1,1))
  g2<-annotate_figure(g2,
                      bottom = text_grob(expression(paste('1/2',chi[1]^2,'+1/2',chi[2]^2)), color = "black",size = text.size),
                      left = text_grob('LRTS',color = 'black',rot = 90,size = text.size)
  )
  g<-ggarrange(g1,g2,nrow=2,ncol=1)
  return(g)
}
