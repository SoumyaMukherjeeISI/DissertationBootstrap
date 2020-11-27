rm(list=ls())
x=rnorm(100,2,1)
l=mean(x)-qnorm(0.975)*1/10
u=mean(x)+qnorm(0.975)*1/10
count=0
for(i in 1:1000)
{
	d=rnorm(100,2,1)
	lo=mean(d)-qnorm(0.975)*1/10
	up=mean(x)+qnorm(0.975)*1/10
	if(lo<2 & up>2)
	{
		count=count+1
	}
}
count/1000