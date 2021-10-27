##p7 p8
d=data.frame(序号=c(1,2,3,4,5,6,7,8,9,10), 
               性别=c("F","F","F","F","F","M","M","M","M","M"),
               年龄=c(14,16,15,17,15,14,16,14,15,16) ,
               "身高/cm"=c(156,158,161,156,153,162,157,159,163,165) ,
               "体重/kg"=c(42.3,45.0,48.5,51.5,44.6,48.8,46.7,49.9,50.2,53.7))
write.table(d, file="C:/Users/hs/Desktop/r/78.txt", row.names=F,quote=F)
setwd("C:/Users/hs/Desktop/r")
d2=read.table("78.txt")
write.csv(d, file="C:/Users/hs/Desktop/r/78.csv", row.names=F,quote=F)

##22  23
### 分块
par(mfrow=c(1,2))
set.seed(281)
##生成数据
x=rnorm(1000)
##绘制图像
hist(x,xlim=c(min(x),max(x)),probability=T,nclass=100,col='lightblue', main='norm distribution')
##密度估计曲线
lines(density(x,bw=1),col='red',lwd=3)

##使用格子点
##先对x轴分块
N=seq(-4,4,length(1000))
##创建函数
f=function(x) +dnorm(x)/sum(dnorm(x))
##调用函数形成随机抽样的分布
f1=f(N)
##T：有放回   N有多少个 n抽多少个
ru1=sample(N,replace=T,size = 1000,prob = f1)  
hist(x,xlim=c(min(x),max(x)),probability=T,nclass=100,col='lightblue', main='norm distribution with sampling')
lines(density(x,bw=1),col='red',lwd=3)
