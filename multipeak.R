ach=a[,1]
con=a[,2]
emirate=con*ach
hist(emirate)
con.large=con[which(con>0.06)]
hist(con.large,main="",xlab = "Concentration of formaldehyde",col="light blue",freq=F)
xfit<-seq(min(con.large),max(con.large),length=40)
yfit=dnorm(xfit,mean(con.large),sd(con.large))
lines(xfit,yfit,col="red",lwd=3)
lines(density(con.large),lwd=2)

#fit two peak Gaussians
## compute cdf
ran=range(con)
breaks=seq(0,0.16,0.01)
con.cut=cut(con,breaks,right = F)
con.freq=table(con.cut)
freq=cbind(con.freq)
n=0
cdf=t(seq(1,nrow(freq),1))
for (i in 1:nrow(freq))
{
  cdf[i]=freq[i]/colSums(freq)
}


# fit two peak Gaussians
con.exp=seq(0,0.155,0.01)
cdf=as.vector(cdf)
con.freq=as.vector(con.freq)
df=data.frame(con.exp,con.freq)
#fit = nls(cdf ~ (a1 * exp(-(con.exp-m1)**2/(2 * s1**2)) +a2 * exp(-(con.exp-m2)**2/(2 * s2**2))),start=list(a1=0.1,a2=0.5,m1=0.06,s1=0.006,m2=0.1,s2=0.01),algorithm = "port")
fit = nls(cdf ~ (a1 * exp(-(con.exp-m1)**2/(2 * s1**2)) +a2 * exp(-(con.exp-m2)**2/(2 * s2**2))),start=list(a1=0.1,a2=0.5,m1=0.06,s1=0.006,m2=0.1,s2=0.01),algorithm = "port")
#fit = nls(cdf ~ ((1/(sqrt(2*pi)*s1)) * exp(-(con.exp-m1)**2/(2 * s1**2)) +a2 * exp(-(con.exp-m2)**2/(2 * s2**2))),start=list(a2=0.5,m1=0.06,s1=0.006,m2=0.1,s2=0.01),algorithm = "port")
myfun=function(x) 0.1547 * exp(-(x-0.0294)**2/(2 * 0.0132**2)) +0.0832 * exp(-(x-0.0952)**2/(2 * 0.0235**2))
h=hist(con,col = "light blue")
h$density = h$counts/sum(h$counts)
plot(h,freq = T,col="light blue",main = "",xlab = "Concentration of formaldehyde")
curve(0.1547 * exp(-(x-0.0294)**2/(2 * 0.0132**2)) +0.0832 * exp(-(x-0.0952)**2/(2 * 0.0235**2)),add=T,col="red",lwd=2)


# Natural ventilation, zero stands for null

ventilation[ventilation==0]=NA
boxplot(ventilation,ylim=c(0,1.6),col = "light blue",boxwex=0.5,ylab="Air change rate")
h=hist(ventilation$natural[ventilation$natural<1.5],col = "grey80",xlab = "Air change rate",probability = T,main = "")
h$density=h$counts/sum(h$counts)
plot(h,freq = F,col = "grey80",main = "",xlab = "Air change rate",ylim = c(0,.5))
ach.exp=h$breaks-.05
df.natr=data.frame(ach.exp,c(0,h$counts)/sum(h$counts))
y=df.natr$c.0..h.counts..sum.h.counts.
x=df.natr$ach.exp
fit = nls(y ~ (a1*x * exp(-(log(x)-m1)**2/(2 * s1**2))+a2 * exp(-(x-m2)**2/(2 * s2**2))),start=list(a1=0.5,a2=0.5,m1=0.1,m2=0.9,s1=0.1,s2=0.3),algorithm = "port")
fit = nls(y ~ (a2 * exp(-(x-m2)**2/(2 * s2**2))), start=list(a2=0.5,m2=0.6,s2=0.3),algorithm = "port")
fit = nls(y ~ (a1 * exp(-(x-m1)**2/(2 * s1**2))+a2 * exp(-(x-m2)**2/(2 * s2**2))),start=list(a1=0.1,a2=0.1,m1=0.15,s1=0.3,m2=0.85,s2=0.3),algorithm = "port")
curve(0.4959 * exp(-(x-0.1849)**2/(2 * 0.0371**2)) +0.1845 * exp(-(x-0.5387)**2/(2 * 0.1256**2)),add=T,col="red",lwd=2,xlim=c(0,1),ylim=c(0,.5))


# emission rate

ach=a[,1]
con=a[,2]
plot(ach[ach<1.3],con[ach<1.3],ylab = "Concentration (mg/m3)",xlab = "Air change rat  /h")
abline(v=0.4,col="red")
abline(h=0.1,col="red")
abline(h=0.06,col="blue")
con1=con[ach<1.3]
ach1=ach[ach<1.3]
fit2=nls(con1~a*b/(ach1+b),start = list(a=1,b=0.15),algorithm = "port")
emirate=con*ach
hist(emirate)
