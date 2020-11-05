library(reshape2)
library(ggplot2)
library(readxl)
rm(list=ls())
f <- function(x) {
  r <- quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

p=ggplot(data=pm,aes(factor(city), pm),ylim=c(0,0.15),cex.axis=1.5,cex.lab=15
) + stat_summary(fun.data=f, aes(fill= location), geom="boxplot", position="dodge")+ ylab(expression(paste("PM2.5 concentration"," (",mu,"g/",m^3,")")))+xlab("Location") +coord_cartesian(ylim = c(0, 135))+
  scale_y_continuous(breaks = round(seq(min(0), max(130), by = 20),1))
p=p+theme(axis.title= element_text(size=15, color="black", vjust=0.5, hjust=0.5))
p=p+theme(axis.text.x=element_text(size=15,color = "black",angle= 45,hjust = 0.5,vjust = 0.55),axis.title.x=element_text(size=15,color = "black"))
p=p+theme(panel.border = element_rect(fill=NA, colour = "black", size=0.6),panel.grid.major = element_blank())
p=p+theme(axis.title.x=element_blank())+theme(legend.position=c(0.85,0.85),legend.key.size = unit(2,"line"),legend.text=element_text(size=15),legend.title=element_blank(),axis.title.y=element_text(margin=margin(0,16,0,0)))
p=p+scale_fill_discrete(labels=c("Indoor","Outdoor"))
p=p+theme(panel.background = element_blank())
p=p+theme(axis.title.y=element_text(size=15))
p=p+theme(text = element_text(size=26))
p=p+theme(axis.text.y = element_text(color='black'))
p=p+geom_vline(xintercept = 4.5,color="blue",lwd=1, show.legend=F)
p


jpeg(file = "pm-io.jpeg",width = 6000, height = 4500,quality = 100,res=600)
par(mar=c(8,6,1.5,2))
p
dev.off()


###### indoor CO2
all_data=all_data[all_data$ID!='dbj8',]
a=unique(all_data$season)
b=unique(all_data$city)
b=c(b[9],b[2],b[5],b[8],b[7],b[1],b[6],b[3],b[4])
med_season=data.frame(city=NA,season=NA,avg=NA,std=NA)
med_season_total=data.frame(city=NA,season=NA,season_c=NA,avg=NA,std=NA)
mx=matrix(data=NA,nrow=4,ncol=9)
for(i in 1:length(a))
{
  for (j in 1:length(b))
  {
    med_season$city=j
    med_season$season_c=a[i]
    med_season$season=i
    med_season$avg=mean(all_data$co2[all_data$season==a[i]&all_data$city==b[j]],na.rm = T)
    med_season$std=sd(all_data$co2[all_data$season==a[i]&all_data$city==b[j]],na.rm = T)
    med_season_total=rbind(med_season,med_season_total)
    mx[i,j]=mean(all_data$co2[all_data$season==a[i]&all_data$city==b[j]],na.rm = T)
  }
}
colnames(mx)=b
rownames(mx)=a
mx=mx[c(2:4,1),]
med_season_total=med_season_total[!is.na(med_season_total$city),]
med_season_total$city[med_season_total$season==1]=med_season_total$city[med_season_total$season==1]+0.24
med_season_total$city[med_season_total$season==2]=med_season_total$city[med_season_total$season==2]-0.24
med_season_total$city[med_season_total$season==3]=med_season_total$city[med_season_total$season==3]-0.08
med_season_total$city[med_season_total$season==4]=med_season_total$city[med_season_total$season==4]+0.08
par(mar=c(8,6,1.5,2))
plot(avg~city,data=med_season_total,pch=c(16,17,18,19)[as.numeric(season)], xaxt = "n",cex=1.5,cex.lab=1.5,cex.axis=1.5,
     xlim=c(0.5,9.5),ylim=c(400,1800),col=c('red','green','blue','black')[as.numeric(season)],xlab="",ylab = '')
abline(h=650)

arrows(med_season_total$city,med_season_total$avg-med_season_total$avg,
       med_season_total$city,med_season_total$avg+med_season_total$avg,
       col=c('red','green','blue','black')[as.numeric(med_season_total$season)],
       length=0.05, angle=90, code=3)

axis(1, at=1:9,cex.axis=1.5,las=0,labels=FALSE)

position.x=c(1:9)+0.2
position.x[9]=position.x[9]+0.3
text(x=position.x,y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]),
     labels=labels.x, srt=45, adj=1, xpd=TRUE,cex = 1.2)
title(ylab=expression(paste(CO['2']," concentration"," (ppm)")),cex.lab=1.5,line=2.5)
abline(v=4.5,col='blue',lwd=2)
text(x=2.5,y=1700,labels = "Northern China",cex=1.2)
text(x=6.5,y=1700,labels = "Southern China",cex=1.2)
tapply(pm$pm[pm$location=='pm_in'],pm$city[pm$location=='pm_in'],median)
