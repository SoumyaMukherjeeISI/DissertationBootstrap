rm(list=ls())
library(boot)

##Setting the parameters

set.seed(1234)
mu=0
sigma=1
n=3
B=999
coverage_iter=100

##Drawing the random sample

x=rnorm(n,mu,sigma)

##Classical Point estimates

x_bar=mean(x)
s_sq_1=var(x)#Assuming unknown mean
s_sq_2=sum((x-mu)^2)/n#assuming known mean 

##Classical CIs

l_mean_1=x_bar-qt(0.975,n-1)*sqrt(s_sq_1/n)
u_mean_1=x_bar+qt(0.975,n-1)*sqrt(s_sq_1/n)
l_mean_2=x_bar-qnorm(0.975)*sigma/sqrt(n)
u_mean_2=x_bar+qnorm(0.975)*sigma/sqrt(n)
c(l_mean_1,u_mean_1)
c(l_mean_2,u_mean_2)

##Function to calculate bootsrap sample means and variances 

meansd.fun=function(data,index)
{
	d=data[index]
	c(mean(d),var(d)/length(d))
}
#meansd.fun<-function(data,index){mean(data[index])}

##Non-parametric bootstrap

b_non=boot(x,statistic=meansd.fun,R=B)
b_non
bootestimate_non=mean(b_non$t)
bootestimate_non
hist(b_non$t[,1],main=paste("Bootstrap Distribution for mu=",mu,",sigma=",sigma,",n=",n," and B=",B,sep=""),freq=F)
ci_non=boot.ci(b_non,conf=0.95)
names(ci_non)
ci_non
coverages_non=c(0,0,0,0,0)
for(i in 1:coverage_iter)
{
	d=rnorm(n,mu,sigma)
	boo=boot(d,statistic=meansd.fun,R=B,stype="i")
	confi=boot.ci(boo,conf=0.95)
	if(confi$normal[2]<mu & confi$normal[3]>mu)
	{
		coverages_non[1]=coverages_non[1]+1
	}
	if(confi$basic[4]<mu & confi$basic[5]>mu)
	{
		coverages_non[2]=coverages_non[2]+1
	}
	if(confi$student[4]<mu & confi$student[5]>mu)
	{
		coverages_non[3]=coverages_non[3]+1
	}
	if(confi$percent[4]<mu & confi$percent[5]>mu)
	{
		coverages_non[4]=coverages_non[4]+1
	}
	if(confi$bca[4]<mu & confi$bca[5]>mu)
	{
		coverages_non[5]=coverages_non[5]+1
	}
}
coverages_non

##parametric bootstrap
b_par=boot(x,statistic=meansd.fun,sim="parametric",R=B)
b_par
bootestimate_par=mean(b_par$t)
bootestimate_par
hist(b_par$t[,1],main=paste("Bootstrap Distribution for mu=",mu,",sigma=",sigma,",n=",n," and B=",B,sep=""),freq=F)
ci_par=boot.ci(b_par,conf=0.95)
names(ci_par)
ci_par
coverages_par=c(0,0,0,0,0)
for(i in 1:coverage_iter)
{
	d=rnorm(n,mu,sigma)
	boo=boot(d,statistic=meansd.fun,sim="parametric",R=B,stype="i")
	confi=boot.ci(boo,conf=0.95)
	if(confi$normal[2]<mu & confi$normal[3]>mu)
	{
		coverages_par[1]=coverages_par[1]+1
	}
	if(confi$basic[4]<mu & confi$basic[5]>mu)
	{
		coverages_par[2]=coverages_par[2]+1
	}
	if(confi$student[4]<mu & confi$student[5]>mu)
	{
		coverages_par[3]=coverages_par[3]+1
	}
	if(confi$percent[4]<mu & confi$percent[5]>mu)
	{
		coverages_par[4]=coverages_par[4]+1
	}
	if(confi$bca[4]<mu & confi$bca[5]>mu)
	{
		coverages_par[5]=coverages_par[5]+1
	}
}
coverages_par



