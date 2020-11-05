rm(list=ls())
library(cluster)
library(readxl)
library(rJava,xlsxjars)
library(ggplot2)
library(data.table)
data.integration=function(data1,data2){
  data1=data.table(data1,key=names(data1)[1:3])
  
  data2=data2[,c(1,2,3,12,13,14)]
  data2=data.table(data2,key=names(data1)[1:3])
  data2=data2[!is.na(data2$ID)]
  
  result=merge(data1,data2,all=T)
  result=as.data.frame(result)
  print((nrow(result)-nrow(data1))/nrow(data2))
  return(result)
}




all_data=rbind(CQ.hour,DB.hour,KM.hour,LG.hour,TJ.hour,TJJ.hour,XA.hour,XJ.hour)
all_data$ID=as.character(all_data$ID)
all_data$pm_in[all_data$pm_in>800]=NA
all_data$pm_out[all_data$pm_out==0]=NA
all_data$pm_in[all_data$pm_in==0]=NA

name=unique(all_data$ID)

all_data=all_data[all_data$time_index>=30,]
all_data=all_data[all_data$ID!='»§Íâ',]
all_data$time=as.POSIXlt(all_data$time,format='%Y-%m-%d %H:%M:%S')

all_data$rh_in=all_data$rh_in/100
all_data$factor=8.35*(all_data$rh_in)^2-7.72*all_data$rh_in+2.8
all_data$factor[all_data$rh_in<=0.5]=1
all_data$pm_in=all_data$pm_in/all_data$factor
all_data$pm_out[all_data$pm_out==0]=NA
all_data$pm_in[all_data$pm_in==0]=NA




rm(CQ.hour,DB.hour,KM.hour,LG.hour,TJ.hour,TJJ.hour,XA.hour,XJ.hour)

all_data[,16]=ven_type[match(all_data$ID,ven_type$Name),6]

###change seldom use occupant to NV


name.high=c('TJJ39','XAJ3','sj7','CJ1','KMJ1','SZJJ04','NNCZ01')
all_data$Type[all_data$ID %in% name.seldom]='NV'
all_data$Type[all_data$ID %in% name.high]=NA
all_data$Type[all_data$Type=='Neg-Mech. ']='NV'


###»­Í¼
all_data$io=all_data$pm_in/all_data$pm_out
all_data$out.level=1
all_data$out.level[all_data$pm_out>=75&all_data$pm_out<150]=2
all_data$out.level[all_data$pm_out>=150]=3



all_data.ap=rbind(CQ.hour.ap,DB.hour.ap,KM.hour.ap,LG.hour.ap,TJ.hour.ap,TJJ.hour.ap,XA.hour.ap,XJ.hour.ap)
all_data=data.integration(all_data,all_data.ap)
all_data=all_data[!(all_data$Type=='NV+AP'&all_data$status==0),]



sc.io=all_data[all_data$zone=='sc',c(16,17,18,1)]
c.io=all_data[all_data$zone=='c',c(16,17,18,1)]
hscw.io=all_data[all_data$zone=='hscw',c(16,17,18,1)]

#all_data$out.level[all_data$pm_out>=75]=1
m.io=all_data[all_data$zone=='m',c(16,17,18)]
hsww.io=all_data[all_data$zone=='hsww',c(16,17,18)]
south.io=rbind(m.io,hsww.io)


f <- function(x) {
  r <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.90))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}



ioplot=function(oridata){
  oridata=oridata[oridata$Type!='seldom',]
  fdata=oridata[!is.na(oridata$out.level),]
  fdata=fdata[fdata$Type %in% c('NV','NV+AP','Pos-Mech.'),]
  p=ggplot(data=fdata,aes(factor(out.level), io),cex.axis=1.5,cex.lab=15) +
     stat_summary(fun.data=f, aes(fill= Type, width=0.5), geom="boxplot", position=position_dodge(0.75))+ 
    ylab('I/O ratio of PM2.5 concentration')+xlab(expression(paste("Outdoor PM2.5 concentration",' (',mu,g/m^3,')'))) + 
    coord_cartesian(ylim = c(0, 2))  #+ geom_boxplot(aes(fill = Condition)) 
  #p=p+theme(axis.title= element_text(size=16, color="black", vjust=0.5, hjust=0.5))
  p=p+scale_x_discrete(labels=c('< 75', '75 - 150', '> 150'),limits=c(1,2,3))+theme(panel.grid.major=element_line())+ theme(legend.key = element_blank())
  #p=p+scale_fill_discrete(labels=c('1','2','d'),limits=c('NV','NV+AP','Pos-Mech.'))
  p=p+theme(panel.background = element_blank())
  #p=p+ scale_fill_brewer(palette='RdBu') + theme_classic()
  p=p+theme(axis.title.x=element_blank())+theme(legend.position=c(0.85,0.83),legend.key.size = unit(1.5,"line"),legend.text=element_text(size=10),legend.title=element_blank(),axis.title.y=element_text(margin=margin(30,16,0,2)))
  p=p+theme(axis.text.y = element_text(colour = 'black',size = 16, margin = margin(0,0.3,0.2,0, unit = "cm")),axis.title.y = element_text(size = 16,color='black'),axis.title.x=element_text(margin=margin(10,16,10,20)))
  p=p+theme(panel.border = element_rect(fill=NA, colour = "black", size=0.6),panel.grid.major = element_blank())
  p=p+theme(axis.text.x=element_text(color = "black",angle= 0,hjust = 0.5,size=16,margin = margin(0.3, unit = "cm")),axis.ticks.length = unit(.25,'cm'),axis.title.x=element_text(color = "black",size=16,vjust = 0.9))
  p=p+ scale_fill_manual(values = c("#b7d1c2", "#ecc6be", "#bdc3ee"),labels=c('NV','PAC','MV'),limits=c('NV','NV+AP','Pos-Mech.'))
  #p=p+geom_hline(yintercept = c(0.41, 0.74))#, 0.57,0.68,0.8,0.95))
  p
}





 ioplot(sc.io)
 ioplot(c.io)
 ioplot(hscw.io)
 ioplot.south(south.io)


jpeg(file = "io_sc.jpeg",width = 4800, height = 3600,quality = 100,res=600)
ioplot(sc.io)
dev.off()



tapply(sc.io$io[sc.io$Type=='NV'],sc.io$out.level[sc.io$Type=='NV'],median,na.rm=T)
tapply(sc.io$io[sc.io$Type=='NV+AP'],sc.io$out.level[sc.io$Type=='NV+AP'],median,na.rm=T)

