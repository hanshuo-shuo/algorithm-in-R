setwd("F:/回归分析R语言程序/6.6")
data=read.csv("6.6R.csv")
n=nrow(data)
attach(data)   #导入data中的数据
lm123=lm(y~x1+x2+x3,data)       #分别建立全子集回归模型
lm12=lm(y~x1+x2)
lm13=lm(y~x1+x3)
lm23=lm(y~x2+x3)
lm1=lm(y~x1)
lm2=lm(y~x2)
lm3=lm(y~x3)

X=list(lm123=lm123,lm12=lm12,lm13=lm13,lm23=lm23,lm1=lm1,lm2=lm2,lm3=lm3)
SSE123=sum(lm123$res^2)      #求残差平方和
SSE12=sum(lm12$res^2)
SSE13=sum(lm13$res^2)
SSE23=sum(lm23$res^2)
SSE1=sum(lm1$res^2)
SSE2=sum(lm2$res^2)
SSE3=sum(lm3$res^2)


RSSE123=SSE123/(n-4)         #求平均残差平方和
RSSE12=SSE12/(n-4)
RSSE13=SSE13/(n-4)
RSSE23=SSE23/(n-4)
RSSE1=SSE1/(n-4)
RSSE2=SSE2/(n-4)
RSSE3=SSE3/(n-4)


cp123=SSE123/(SSE123/(n-4))+2*4-n     #求cp值
cp12=SSE12/(SSE123/(n-4))+2*3-n 
cp13=SSE13/(SSE123/(n-4))+2*3-n 
cp23=SSE23/(SSE123/(n-4))+2*3-n 
cp1=SSE1/(SSE123/(n-4))+2*2-n 
cp2=SSE2/(SSE123/(n-4))+2*2-n 
cp3=SSE3/(SSE123/(n-4))+2*2-n 




AIC123=n*log(SSE123)+2*4              
AIC12=n*log(SSE12)+2*3
AIC13=n*log(SSE13)+2*3
AIC23=n*log(SSE23)+2*3
AIC1=n*log(SSE1)+2*2
AIC2=n*log(SSE2)+2*2
AIC3=n*log(SSE3)+2*2


RSSE=c(RSSE123,RSSE12,RSSE13,RSSE23,RSSE1,RSSE2,RSSE3)
cp=c(cp123,cp12,cp13,cp23,cp1,cp2,cp3)
AIC=c(AIC123,AIC12,AIC13,AIC23,AIC1,AIC2,AIC3)
RSSR.sort=list(X[order(RSSE)])    #按RSSE准则，对全子集回归模型升序排序
cp.sort=list(X[order(cp)])        #按cp准则，对全子集回归模型升序排序
AIC.sort=list(X[order(AIC)])      #按AIC准则，对全子集回归模型升序排序



################    output the result    ##########################
sink("ex6.6_result.txt")
cat("The result for RSSE","\n")
cp.sort
cat("\n")


cat("The result for cp","\n")
cp.sort
cat("\n")

cat("The result for AIC","\n")
AIC.sort
cat("\n")

sink()