rm(list=ls())
set.seed(1234)
blue=c(4,69,87,35,39,79,31,79,65,95,68,62,70,80,84,79,66,75,59,77,36,86,39,85,74,72,69,85,85,72)
red=c(62,80,82,83,0,81,28,69,48,90,63,77,0,55,83,85,54,72,58,68,88,83,78,30,58,45,78,64,87,65)
length(blue)
length(red)
acui=data.frame(str=c(rep(0,20),rep(1,10)),red,blue)
acui

#Classical Students t

acu.pd=acui[acui$str==0,]
dif=acu.pd$blue-acu.pd$red
n=nrow(acu.pd)
tmp=qt(0.025,n-1)*sd(dif)/sqrt(n)
c(mean(dif)+tmp,mean(dif)-tmp)

#boot cis
library(boot)
set.seed(1234)
acu.pd.fun=function(data,index)
{
	d=data[index,]
	dif=d$blue-d$red
	c(mean(dif),var(dif)/length(dif))
}
acu.pd.b=boot(acu.pd,acu.pd.fun,R=999)
boot.ci(acu.pd.b)
