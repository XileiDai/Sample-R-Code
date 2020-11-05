rm(list=ls())
library(cluster)
library(readxl)
library(rJava,xlsxjars)
library(ggplot2)

all_data=rbind(CQ.hour,XJ.hour,KM.hour,LG.hour,TJ.hour,TJJ.hour,XA.hour,DB.hour)
all_data$ID=as.character(all_data$ID)
all_data$pm_in[all_data$pm_in>800]=NA
all_data$pm_out[all_data$pm_out==0]=NA
all_data$pm_in[all_data$pm_in==0]=NA
name=unique(all_data$ID)
all_data=all_data[all_data$room=='bed',]
all_data=all_data[all_data$time_index>=30,]
all_data=all_data[all_data$ID!='ЛЇЭт',]
all_data$time=as.POSIXlt(all_data$time,format='%Y-%m-%d %H:%M:%S')
all_data$night[all_data$time$hour>=0&all_data$time$hour<=6]='night'
all_data=all_data[which(all_data$night=='night'),]
all_data$rh_in=all_data$rh_in/100
all_data$factor=8.35*(all_data$rh_in)^2-7.72*all_data$rh_in+2.8
all_data$factor[all_data$rh_in<=0.5]=1
all_data$pm_in=all_data$pm_in/all_data$factor
all_data$pm_out[all_data$pm_out==0]=NA
all_data$pm_in[all_data$pm_in==0]=NA




all_data$floor=NA
all_data$Vol=NA
all_data$occupant=NA
all_data[,16]=Basic_info[match(all_data$ID,Basic_info$No.),3]
all_data[,17]=Basic_info[match(all_data$ID,Basic_info$No.),4]
all_data[,18]=Basic_info[match(all_data$ID,Basic_info$No.),5]
all_data$date=cut(all_data$time,breaks = 'day')
Nmean=function(index,Oridata)
{
  lapply(index,function(i){
    x=Oridata[Oridata$ID==i,]
    co2=tapply(Oridata$co2[Oridata$ID==i],Oridata$date[Oridata$ID==i],mean,na.rm=T)
    tem=tapply(Oridata$tem_out[Oridata$ID==i],Oridata$date[Oridata$ID==i],mean,na.rm=T)
    rh=tapply(Oridata$rh_out[Oridata$ID==i],Oridata$date[Oridata$ID==i],mean,na.rm=T)
    pm_out=tapply(Oridata$pm_out[Oridata$ID==i],Oridata$date[Oridata$ID==i],mean,na.rm=T)
    pm_in=tapply(Oridata$pm_in[Oridata$ID==i],Oridata$date[Oridata$ID==i],mean,na.rm=T)
    floor=tapply(Oridata$floor[Oridata$ID==i],Oridata$date[Oridata$ID==i],mean,na.rm=T)
    Vol=tapply(Oridata$Vol[Oridata$ID==i],Oridata$date[Oridata$ID==i],mean,na.rm=T)
    occupant=tapply(Oridata$occupant[Oridata$ID==i],Oridata$date[Oridata$ID==i],mean,na.rm=T)
    name=vector(length = length(co2))
    zone=vector(length=length(co2))
    city=vector(length = length(co2))
    name[1:length(name)]=i
    zone[1:length(zone)]=x$zone[1]
    city[1:length(city)]=x$city[1]
    return(data.frame(name,city,zone,co2,tem,rh,pm_out,pm_in,floor,Vol,occupant))
  })
}
all_data.day=do.call('rbind',Nmean(unique(all_data$ID),all_data))
#oridata=all_data.day[all_data.day$zone==clizone1,]
co2.cor=cor(na.omit(all_data.day[,c(2,3,4,5,7,8,9)+2]))
pm.cor=cor(na.omit(all_data.day[,c(6,3,4,5,7,8,9)+2]))






#########co2 Vs out_pm
co2_pm=function(clizone,span_index,xlim,ylim1,ylim2,leg.x){
all_data_zone=all_data.day[all_data.day$zone==clizone,]
q_down=quantile(all_data_zone$pm_out,0.05,na.rm=T)
q_up=quantile(all_data_zone$pm_out,0.95,na.rm=T)

all_data_zone[,12]=ven_type[match(all_data_zone$name,ven_type$Name),6]

#change seldom use occupant to NV

all_data_zone$Type[all_data_zone$name %in% name.seldom]='NV'
all_data_zone$Type[all_data_zone$Type=='Neg-Mech. ']='NV'

unique(all_data_zone$Type)
#  "NV"        "Pos-Mech." "NV+AP"   'Pos-Mech.+AP 
all_data_zone$name=as.character(all_data_zone$name)

co2.pm=function(type){
a=all_data_zone[all_data_zone$Type==type,]
a=a[order(a$pm_out),]
a=a[!is.na(a$pm_out)&!is.na(a$co2),]
return(a)
}

par(mar=c(4,4,0.5,0.5),mgp=c(2.5,1,0))

dxl=co2.pm('NV+AP')
dxl=dxl[dxl$co2>500,]
#q10=quantile(dxl$pm_out,0.05)
#q90=quantile(dxl$pm_out,0.95)
#dxl=dxl[dxl$pm_out>q10&dxl$pm_out<q90,]
plx=predict(loess(co2~pm_out,data=dxl,span = span_index),se=T)


plot(dxl$pm_out,plx$fit,ylim=c(ylim1,ylim2),xlim=c(35,q_up),
     col=rgb(0,159,194,max=255),lwd=2,ylab=expression(paste(CO[2],' concentration (ppm)')),xlab=expression(paste("Outdoor PM2.5 concentration"," (",mu,"g/",m^3,")"))
     ,cex.lab=1.2,cex.axis=1.2,type='l')

low=plx$fit - qt(0.95,plx$df)*plx$se
high=plx$fit + qt(0.95,plx$df)*plx$se
lines(dxl$pm_out,plx$fit - qt(0.95,plx$df)*plx$se, lty=2,col=rgb(0,159,194,max=255),lwd=2)
lines(dxl$pm_out,plx$fit + qt(0.95,plx$df)*plx$se, lty=2,col=rgb(0,159,194,max=255),lwd=2)
polygon(c(dxl$pm_out, rev(dxl$pm_out)), c(high, rev(low)),
        col=rgb(0,159,194,max=255,alpha = 80), border = NA)


dxl=co2.pm('NV')
dxl=dxl[dxl$co2>500,]
#q10=quantile(dxl$pm_out,0.05)
#q90=quantile(dxl$pm_out,0.95)
#dxl=dxl[dxl$pm_out>q10&dxl$pm_out<q90,]
plx=predict(loess(co2~pm_out,data=dxl,span = span_index),se=T)
lines(dxl$pm_out,plx$fit,
      type = 'l',col=rgb(147,111,177,max=255),lwd=2)
low=plx$fit - qt(0.95,plx$df)*plx$se
high=plx$fit + qt(0.95,plx$df)*plx$se
lines(dxl$pm_out,plx$fit - qt(0.95,plx$df)*plx$se, lty=2,col=rgb(147,111,177,max=255),lwd=2)
lines(dxl$pm_out,plx$fit + qt(0.95,plx$df)*plx$se, lty=2,col=rgb(147,111,177,max=255),lwd=2)
polygon(c(dxl$pm_out, rev(dxl$pm_out)), c(high, rev(low)),
        col=rgb(147,111,177,max=255,alpha = 80), border = NA)

dxl=co2.pm('Pos-Mech.')
dxl=dxl[dxl$co2>500,]
#q10=quantile(dxl$pm_out,0.05)
#q90=quantile(dxl$pm_out,0.95)
#dxl=dxl[dxl$pm_out>q10&dxl$pm_out<q90,]
plx=predict(loess(co2~pm_out,data=dxl,span = span_index),se=T)
lines(dxl$pm_out,plx$fit,type = 'l',col=rgb(200,0,0,max=255),lwd=2)
low=plx$fit - qt(0.95,plx$df)*plx$se
high=plx$fit + qt(0.95,plx$df)*plx$se
lines(dxl$pm_out,plx$fit - qt(0.95,plx$df)*plx$se, lty=2,col=rgb(200,0,0,max=255),lwd=2)
lines(dxl$pm_out,plx$fit + qt(0.95,plx$df)*plx$se, lty=2,col=rgb(200,0,0,max=255),lwd=2)
polygon(c(dxl$pm_out, rev(dxl$pm_out)), c(high, rev(low)),
        col=rgb(200,0,0,max=255,alpha = 80), border = NA)
}


co2_pm('sc',0.85,500,550,1400,140)
abline(h=c(750,1250,1000,1150),v=c(60))
co2_pm('c',0.85,500,700,1200,150)
abline(v=c(60,115),h=c(1000))
co2_pm('hscw',0.85,85,700,1300,150)


jpeg(file = "co2_pm_sc.jpeg",width = 4800, height = 3600,quality = 100,res=600)
co2_pm('sc',0.85,150,550,1400,140)
legend(32,1430,c("PAC","NV","MV"),lty=1,lwd=2,col=c(rgb(0,159,194,max=255),rgb(147,111,177,max=255),rgb(200,0,0,max=255)),bty='n')
dev.off()



