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
