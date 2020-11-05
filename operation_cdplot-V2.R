library(rJava)
library(xlsxjars)
library(xlsx)
library(readxl)
library(data.table)
library(MASS)
rm(list=ls())
table(a$winter==0)[1]/(table(a$winter==0)[1]+table(a$winter==0)[2])
table(a$C_winter==0)[1]/(table(a$C_winter==0)[1]+table(a$C_winter==0)[2])
table(a$HSCW_winter==0)[1]/(table(a$HSCW_winter==0)[1]+table(a$HSCW_winter==0)[2])
table(a$M_winter==0)[1]/(table(a$M_winter==0)[1]+table(a$M_winter==0)[2])

a[a<2]=NA
plot(density(a$winter,na.rm=T),xlim=c(0,1440))
plot(density(a$C_winter,na.rm=T),xlim=c(0,1440))
plot(density(a$HSCW_winter,na.rm=T),xlim=c(0,1440))
plot(density(a$M_winter,na.rm=T),xlim=c(0,1440))

par(new=F)
par(mar=c(4,5,1.5,2))
par(mfcol=c(2,2)) 
hist(a$winter,probability = T,ylim=c(0,0.013),xlab = "Open-window",main = "SC zone",
     col='grey70',cex.lab=1.2,cex.axis=1.3,xlim=c(0,1440))
x=seq(0,1440,length=2000)
para=fitdistr(na.omit(a$winter),densfun = 'lognormal')
y=dlnorm(x,mean=para$estimate[1],sd=para$estimate[2])
x[which(y==max(y))]
points(x,y,type="l",col="red",lwd=2)


