power_beta<-function(data,p,mix,thres=32.491245){
  dat<-data[data$maf==p & data$mix_param==mix,]
  beta <- c(-1,-.9,-.8,-.7,-.6,-.5,-.4,-.3,-.2,-.1,0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)

  plot(beta,sapply(beta,mix_power, data=dat,thres=thres),col='red',type = 'l',axes = FALSE,lwd = 2,ylim = c(0,1))
  lines(beta,sapply(beta,logit_power,data=dat),col='blue',lwd=2)
}

mix_power<-function(data,beta,thres){
  mean(data$lik_ratio_stat[data$true_beta==beta]>thres)
}

logit_power<-function(data,beta){
  mean(data$lik_ratio_logit[data$true_beta==beta]>29.71679)
}

plot_power_grid<-function(data,maf,mix,thres,mag=1.5){
  op <- par(cex.lab=1.5, cex.axis=mag, cex.main=mag, cex.sub=mag,mfrow = c(3,3),
            oma = c(12,13,3,0) + 0.1,
            mar = c(0,0,1,1) + 0.1,
            mgp=c(3,2,0),las=3)
  lp<--1.4 #position of left text

  power_beta(data = data,p = maf[1],mix = mix[1],thres = thres)
  Axis(side=1, at = seq(-1,1,0.1),labels=FALSE)
  Axis(side=2)
  par(xpd=NA)
  text(lp,0.5,paste('maf = ',maf[1],sep=''),cex = mag,col = 'red',srt=90)
  text(0,0.9,bquote(alpha==.(mix[1])),cex=mag)
  par(xpd=F)

  power_beta(data = data,p = maf[1],mix = mix[2],thres = thres)
  Axis(side=1, at = seq(-1,1,0.1),labels=FALSE)
  Axis(side=2,labels = F)
  text(0,0.9,bquote(alpha==.(mix[2])),cex=mag)

  power_beta(data = data,p = maf[1],mix = mix[3],thres = thres)
  Axis(side=1, at = seq(-1,1,0.1),labels=FALSE)
  Axis(side=2,labels = F)
  text(0,0.9,bquote(alpha==.(mix[3])),cex=mag)

  power_beta(data = data,p = maf[2],mix = mix[1],thres = thres)
  Axis(side=1, at = seq(-1,1,0.1),labels=FALSE)
  Axis(side=2)
  par(xpd=NA)
  text(lp,0.5,paste('maf = ',maf[2],sep=''),cex = mag,col = 'red',srt=90)
  par(xpd=F)

  power_beta(data = data,p = maf[2],mix = mix[2],thres = thres)
  Axis(side=1, at = seq(-1,1,0.1),labels=FALSE)
  Axis(side=2,labels = F)

  power_beta(data = data,p = maf[2],mix = mix[3],thres = thres)
  Axis(side=1, at = seq(-1,1,0.1),labels=FALSE)
  Axis(side=2,labels = F)

  power_beta(data = data,p = maf[3],mix = mix[1],thres = thres)
  Axis(side=1, at = seq(-1,1,0.2))
  Axis(side=1, at = seq(-1,1,0.1),labels = F)
  Axis(side=2)
  par(xpd=NA)
  text(lp,0.5,paste('maf = ',maf[3],sep=''),cex = mag,col = 'red',srt=90)
  par(xpd=F)

  power_beta(data = data,p = maf[3],mix = mix[2],thres = thres)
  Axis(side=1, at = seq(-1,1,0.2))
  Axis(side=1, at = seq(-1,1,0.1),labels = F)
  Axis(side=2,labels = F)

  power_beta(data = data,p = maf[3],mix = mix[3],thres = thres)
  Axis(side=1, at = seq(-1,1,0.2))
  Axis(side=1, at = seq(-1,1,0.1),labels = F)
  Axis(side=2,labels = F)
  par(xpd=NA)
  legend('bottomright',inset = c(0,-0.7),
         legend = c("Mixture", "Logistic"),
         col=c('red','blue'), lwd=5, cex=mag, horiz = TRUE,bty = 'n')
  par(xpd=F)
  par(cex=1.5)
  title(xlab = "Effect Size",
        ylab = "Power",
        outer = TRUE, line = 9,cex=mag)
}
