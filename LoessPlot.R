LoessPlot = function(Var1, Var2, span){
  oridata = data.frame(Var1 = Var1, Var2 = Var2)
  q10 = quantile(Var1, 0.05, na.rm = T)
  q90 = quantile(Var1, 0.95, na.rm = T)
  oridata=oridata[oridata$Var1>q10&oridata$Var1<q90,]
  oridata=oridata[order(oridata$Var1),]
  oridata=oridata[!is.na(oridata$Var1)&!is.na(oridata$Var2),]
  plx=predict(loess(Var2~Var1, data = oridata, span = span),se=T)
  low=plx$fit - qt(0.95,plx$df)*plx$se
  high=plx$fit + qt(0.95,plx$df)*plx$se
  #plot.new()
  plot(oridata$Var1,plx$fit,type = 'l',col=rgb(200,0,0,max=255),lwd=2, ylim = c(min(low),max(high)))
  lines(oridata$Var1,plx$fit - qt(0.95,plx$df)*plx$se, lty=2,col=rgb(200,0,0,max=255),lwd=2)
  lines(oridata$Var1,plx$fit + qt(0.95,plx$df)*plx$se, lty=2,col=rgb(200,0,0,max=255),lwd=2)
  polygon(c(oridata$Var1, rev(oridata$Var1)), c(high, rev(low)),
          col=rgb(200,0,0,max=255,alpha = 80), border = NA)
  #legend(-14,630,c("SC zone","C zone"),lty=1,lwd=2,col=c(rgb(0,159,194,max=255),rgb(200,0,0,max=255)))
}
