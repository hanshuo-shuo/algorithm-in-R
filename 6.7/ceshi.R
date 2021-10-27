setwd("F:/�ع����R���Գ���/6.7")
data=read.csv("6.7R.csv")
n=nrow(data)
attach(data)   #����data�е�����
n = length(y)
x1.square = x1^2
lm.1 = lm(y ~ x1.square + x2)
y.res = rstudent(lm.1)
plot(lm.1$fit, y.res, col = 2, pch = 20, cex = 1.5, xlab = "fitted value", ylab = "rstudent", main = "rstudent plot" )
abline(h = 0, lty = 6)
lambda = rep(0, 1000)
RSS    = rep(0, 1000)
for (i in 1 : 1000 )
{
  lambda[i] = 0.001 * i
  y_lambda = ((y^lambda[i] - 1)/lambda[i])/((cumprod(y)[n])^((lambda[i] - 1)/n))
  a = lm(y_lambda~x1.square + x2)
  RSS[i] = sum(a$res^2)
}

plot(lambda, RSS, pch = 17, col = 4, main = "boxcox")
index = which.min(RSS)
lambda.best = index * 0.001#应该选取的lambda的�?
y_ = (y^lambda.best - 1)/lambda.best
lm.new = lm(y_ ~ x1.square + x2)
plot(lm.new$fit, rstudent(lm.new), col = 6, pch = 20, cex = 1.5, xlab = "fitted value", ylab = "rstudent", main = "rstudent  plot" )
abline(h = 0, lty = 6)

Rss.old = sum(lm.1$res^2)#变换前的残差平方�?
Rss.new = sum(lm.new$res^2)#变换后的残差平方�?
beta.old = coef(lm.1) #变换前的系数
beta.new = coef(lm.new) #变换后的系数
#因此确定了lambda的值和变换之后自变量的系数�?

