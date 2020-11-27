rm(list=ls())
library(boot)
set.seed(1234)
rate=1
lamda=1/rate
n=10
B=999
x=rexp(n,rate)
mean.fun<-function(data,index){mean(data[index])}
b=boot(x,statistic=mean.fun,R=B,stype="i")
b
bootestimate=mean(b$t)
bootestimate
hist(b$t,main=paste("Bootstrap Distribution for lamda=",lamda,",n=",n," and B=",B,sep=""),freq=F)
ci=boot.ci(b,conf=0.95)
names(ci)
coverages=c(0,0,0,0)
for(i in 1:1000)
{
	d=rexp(n,rate)
	boo=boot(d,statistic=mean.fun,R=B,stype="i")
	confi=boot.ci(boo,conf=0.95)
	if(confi$normal[2]<lamda & confi$normal[3]>lamda)
	{
		coverages[1]=coverages[1]+1
	}
	if(confi$basic[4]<lamda & confi$basic[5]>lamda)
	{
		coverages[2]=coverages[2]+1
	}
	if(confi$percent[4]<lamda & confi$percent[5]>lamda)
	{
		coverages[3]=coverages[3]+1
	}
	if(confi$bca[4]<lamda & confi$bca[5]>lamda)
	{
		coverages[4]=coverages[4]+1
	}
}
coverages
