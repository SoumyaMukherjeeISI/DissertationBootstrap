rm(list=ls())
library(boot)
set.seed(1234)
m=10
p=0.5
n=5
B=2000
x=rbinom(n,m,p)
mean.fun<-function(data,index){mean(data[index])}
b=boot(x,statistic=mean.fun,R=B,stype="i")
b
bootestimate=mean(b$t)
bootestimate
hist(b$t,main=paste("Bootstrap Distribution for m=",m,",p=",p,",n=",n," and B=",B,sep=""),freq=F)
ci=boot.ci(b,conf=0.95)
names(ci)
coverages=c(0,0,0,0)
for(i in 1:1000)
{
	d=rbinom(n,m,p)
	boo=boot(d,statistic=mean.fun,R=B,stype="i")
	confi=boot.ci(boo,conf=0.95)
	if(confi$normal[2]<m*p & confi$normal[3]>m*p)
	{
		coverages[1]=coverages[1]+1
	}
	if(confi$basic[4]<m*p & confi$basic[5]>m*p)
	{
		coverages[2]=coverages[2]+1
	}
	if(confi$percent[4]<m*p & confi$percent[5]>m*p)
	{
		coverages[3]=coverages[3]+1
	}
	if(confi$bca[4]<m*p & confi$bca[5]>m*p)
	{
		coverages[4]=coverages[4]+1
	}
}
coverages