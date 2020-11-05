rm(list=ls())
library(cluster)
library(readxl)
library(rJava,xlsxjars)
library(ggplot2)
all_data$ID=as.character(all_data$ID)
all_data$pm_in[all_data$pm_in>800]=NA
all_data$pm_out[all_data$pm_out==0]=NA
all_data$pm_in[all_data$pm_in==0]=NA
name=unique(all_data$ID)
all_data$co2[all_data$room!='bed']=NA  
all_data=all_data[all_data$time_index>=30,]
all_data=all_data[all_data$ID!='ЛЇЭт',]
all_data$time=as.POSIXlt(all_data$time,format='%Y-%m-%d %H:%M:%S')
all_data$night[all_data$time$hour>=0&all_data$time$hour<=6]='night'
#all_data=all_data[which(all_data$night=='night'),]
all_data$rh_in=all_data$rh_in/100
all_data$factor=8.35*(all_data$rh_in)^2-7.72*all_data$rh_in+2.8
all_data$factor[all_data$rh_in<=0.5]=1
all_data$pm_in=all_data$pm_in/all_data$factor
all_data$pm_out[all_data$pm_out==0]=NA
all_data$pm_in[all_data$pm_in==0]=NA

all_data.night=all_data[which(all_data$night=='night'),]
all_data.night$day=cut(all_data.night$time,breaks = 'day')
all_data$day=cut(all_data$time,breaks = 'day')
rm(CQ.hour,DB.hour,KM.hour,LG.hour,TJ.hour,TJJ.hour,XA.hour,XJ.hour)

mean_byid=function(index,oridata){
  lapply(index,function(i){
    x=oridata[oridata$ID==i,]
    co2=tapply(oridata$co2[oridata$ID==i],oridata$day[oridata$ID==i],mean,na.rm=T)
    pm_in=tapply(oridata$pm_in[oridata$ID==i],oridata$day[oridata$ID==i],mean,na.rm=T)
    pm_out=tapply(oridata$pm_out[oridata$ID==i],oridata$day[oridata$ID==i],mean,na.rm=T)
    tem_out=tapply(oridata$tem_out[oridata$ID==i],oridata$day[oridata$ID==i],mean,na.rm=T)
    rh_out=tapply(oridata$rh_out[oridata$ID==i],oridata$day[oridata$ID==i],mean,na.rm=T)
    name=vector(length = length(co2))
    zone=vector(length=length(co2))
    city=vector(length = length(co2))
    season=vector(length = length(co2))
    name[1:length(name)]=i
    zone[1:length(zone)]=x$zone[1]
    city[1:length(city)]=x$city[1]
    time=names(co2)
    return(data.frame(name,city,zone,co2,pm_in,pm_out,tem_out,rh_out,time))
  })
}
night_avg=do.call('rbind',mean_byid(unique(all_data.night$ID),all_data.night))
wholeday_avg=do.call('rbind',mean_byid(unique(all_data$ID),all_data))
night_avg$time=as.POSIXct(night_avg$time,format='%Y-%m-%d')
wholeday_avg$time=as.POSIXct(wholeday_avg$time,format='%Y-%m-%d')
night_avg=clseason(night_avg)
wholeday_avg=clseason(wholeday_avg)
wholeday_avg$io=wholeday_avg$pm_in/wholeday_avg$pm_out

sc.pm=tapply(wholeday_avg$pm_in[wholeday_avg$zone=='sc'],wholeday_avg$season[wholeday_avg$zone=='sc'],quantile,c(0.1,0.5,0.9),na.rm=T)

sc.io=tapply(wholeday_avg$io[wholeday_avg$zone=='sc'],wholeday_avg$season[wholeday_avg$zone=='sc'],quantile,c(0.1,0.5,0.9),na.rm=T)

io.winter.south=wholeday_avg[wholeday_avg$zone=='hscw'|wholeday_avg$zone=='m'|wholeday_avg$zone=='hsww',]
wilcox.test(io.winter.north$io[io.winter.north$season=='summer'],io.winter.south$io[io.winter.south$season=='summer'])




sc.co2=tapply(wholeday_avg$co2[wholeday_avg$zone=='sc'],wholeday_avg$season[wholeday_avg$zone=='sc'],quantile,c(0.1,0.5,0.9),na.rm=T)

sc.co2.night=tapply(night_avg$co2[night_avg$zone=='sc'],night_avg$season[night_avg$zone=='sc'],quantile,c(0.1,0.5,0.9),na.rm=T)



overstand=function(index,oridata,column,guideline){
  lapply(index,function(i){
    x=oridata[oridata$name==i,]
    overnum=sum(x[,column]>guideline,na.rm=TRUE)
    totalnum=sum(x[,column]>0,na.rm=TRUE)
    rate=overnum/totalnum*100
    return(data.frame(i,rate))
  })
}
overstand.all.co2=do.call('rbind',overstand(unique(night_avg$name),night_avg,4,1000))
overstand.all.pm=do.call('rbind',overstand(unique(wholeday_avg$name),wholeday_avg,5,75))
overstand.all.pm25=do.call('rbind',overstand(unique(wholeday_avg$name),wholeday_avg,5,25))
par(mar=c(5,5,0.5,0.5))



jpeg(file = "percentofoverstandard.jpeg",width = 4800, height = 4000,quality = 100,res=600)
plot(ecdf(overstand.all.co2$rate),col='blue',main="",xlab='Percent',ylab='Cumulative probability',cex=1,cex.axis=1.3,
     cex.lab=1.5,pch=17,ylim=c(0,1))

lines(ecdf(overstand.all.pm$rate),col='red',main="",cex=1,cex.axis=1.3,
     cex.lab=1.5,pch=16)
lines(ecdf(overstand.all.pm25$rate),col='green',main="",cex=1,cex.axis=1.3,
      cex.lab=1.5,pch=16)
abline(h=seq(0,1,0.1),v=seq(0,100,10),col='gray50',lty=2)
a=expression(paste('PM2.5 (',75,' ', mu, g/m^3,') '))
b=expression(paste('PM2.5 (',25,' ', mu, g/m^3,') '))
legend(70,0.2,c(a,b,expression(CO[2])),pch=c(16,16,17),col=c('red','green','blue'))
dev.off()

plot(ecdf(overstand.all.co2$rate),col='blue',main="",xlab='Percent',ylab='Cumulative probability',cex=1,cex.axis=1.3,
     cex.lab=1.5,pch=17,ylim=c(0,1))

lines(ecdf(overstand.all.pm$rate),col='red',main="",cex=1,cex.axis=1.3,
      cex.lab=1.5,pch=16)
lines(ecdf(overstand.all.pm25$rate),col='green',main="",cex=1,cex.axis=1.3,
      cex.lab=1.5,pch=16)


night_avg$type = NA
night_avg[,11]=ven_type[match(night_avg$name,ven_type$Name),6]
night_avg$type[night_avg$name %in% name.seldom]='NV'
night_avg$type[night_avg$type=='Neg-Mech. ']='NV'
unique(night_avg$type)
mean(night_avg$co2[night_avg$zone=='hsww'&night_avg$type=='NV'], na.rm = T)
sd(night_avg$co2[night_avg$zone=='hsww'&night_avg$type=='NV'], na.rm = T)
