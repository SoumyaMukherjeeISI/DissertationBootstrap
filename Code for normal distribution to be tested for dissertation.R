rm(list=ls())
library(boot)
set.seed(10)
mu=0
sigma=1
n=10
B=999
x=rnorm(n,mu,sigma)
mean.fun<-function(data,index){mean(data[index])}
b=boot(x,statistic=mean.fun,R=B,stype="i")
b
bootestimate=mean(b$t)
bootestimate
hist(b$t,main=paste("Bootstrap Distribution for mu=",mu,",sigma=",sigma,",n=",n," and B=",B,sep=""),freq=F)
ci=boot.ci(b,conf=0.95)
names(ci)
coverages=c(0,0,0,0)
for(i in 1:1000)
{
	d=rnorm(n,mu,sigma)
	boo=boot(d,statistic=mean.fun,R=B,stype="i")
	confi=boot.ci(boo,conf=0.95)
	if(confi$normal[2]<mu & confi$normal[3]>mu)
	{
		coverages[1]=coverages[1]+1
	}
	if(confi$basic[4]<mu & confi$basic[5]>mu)
	{
		coverages[2]=coverages[2]+1
	}
	if(confi$percent[4]<mu & confi$percent[5]>mu)
	{
		coverages[3]=coverages[3]+1
	}
	if(confi$bca[4]<mu & confi$bca[5]>mu)
	{
		coverages[4]=coverages[4]+1
	}
}
coverages