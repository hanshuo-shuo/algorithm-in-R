setwd("F:/回归分析R语言程序/6.7")
data=read.csv("6.7R.csv")
n=nrow(data)
attach(data)   #导入data中的数据
fit=lm(y~I(x1^2)+x2)
summary(fit)
studenterror=rstudent(fit)
yest=fit$fitted.values
plot(yest,studenterror,xlab='fitted values')
title(main=' 残差图-原模型')
abline(h=0)### 发现y估计值越小学生化残差越小,呈发散趋势



lambda=rep(0,10000)
RSSE=rep(0,10000)
for(i in 1:10000)
{
  lambda[i] = 0.0001 * i
  z=((y^lambda[i] - 1)/lambda[i])/((cumprod(y)[n])^((lambda[i] - 1)/n))
  lm.z=lm(z~x1.square + x2)
  RSSE[i]=sum(lm.z$res^2)
}

plot(lambda, RSSE,main = "boxcox",type="l")
index=which.min(RSSE)
lambda.best=index*0.0001

ylamd=(y^0.737-1)/0.737
fit3=lm(ylamd~I(x1^2)+x2)
summary(fit3)
studenterror3=rstudent(fit3)
yest3=fit3$fitted.values
plot(yest3,studenterror3,xlab='fitted values',ylab = 'studenterror')
title(main='残差图-变换后')
abline(h=0)
