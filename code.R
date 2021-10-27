#The following codes consist of simulations of global testing with sparsity sp=2.
library(MASS)
library(purrr)
library(glmnet)
library(Rfast)
library(parallel)#加载并行计算包
library(doParallel)
library(foreach)
#设计foreach包的思想可能想要创建一个lapply和for循环的标准
#初始化的过程有些不同，你需要register注册集群

#cores_2_use <- detectCores() - 1
#clusterSetRNGStream(123)
#set.seed(123)

#####Global Testing, Type I Errors

sp=2


p = c(100,200,300,400)
r = c(0.2,0.4,1.2)  ## ratio r = p/n
alpha= 0.05  ##level α = 0.05 based on 1000 simulations
## 把simulations的次数调小一点
q.alpha = -log(pi)-2*log(log(1/(1-alpha)))
numcores=8
BBB=500
batchsize=4
#numcores*BBB/batchsize=1000

tune.1=1.5
tune.2=c(1,1,1)
f = function(x){
  exp(x)/(1+exp(x))
}


power.g = matrix(rep("NA",12),ncol=3, nrow=4)
power.adh = matrix(rep("NA",12),ncol=3, nrow=4)
power.llr= matrix(rep("NA",12),ncol=3, nrow=4)

for(o in 1:length(p)){
  n=as.integer(p[o]/r)
  b = rep(0,p[o])
  sig = toeplitz(seq(0.7,0.7,length.out = p[o]/10))
  Sig = bdiag(rep(list(sig),10))+diag(rep(0.3,p[o])) 
  ## diagonal elements are 1’s and off-diagonal elements are set as 0.7
  
  for(s in 1:(length(r))){
    
    cl <- parallel::makeCluster(numcores, outfile = "debug.txt")# 初始化cpu集群
    #cl <- parallel::makeCluster(cores_2_use, outfile = "")
    clusterSetRNGStream(cl, 123)
    ##这个用来设置种子，在并行计算里面不能用set.seed
    registerDoParallel(cl)
    test.g = rep("NA",batchsize)
    test.adh=rep("NA",batchsize)
    test.llr=rep("NA",batchsize)
    
    G.end=foreach(round=1:(BBB/batchsize)) %dopar% {
      library(MASS)
      library(purrr)
      library(glmnet)
      library(Rfast)
      for(bb in 1:batchsize){
        X = mvrnorm(n[s], mu=rep(0,p[o]), Sigma=Sig)
        
        prob = exp(X %*% b)/(1+exp(X %*% b))
        y = rep(1,n[s])
        while(sum(y)/n[s]<0.02 | sum(y)/n[s]>0.98 ){
          for(gen.y in 1:n[s]){
            y[gen.y]=rbinom(1,1,prob[gen.y])
          }
        }
        
        ####### biased estimation
        
        my.logistic.fit = glmnet(x = X, y = y, family = "binomial", alpha = 1,  intercept=F, lambda = 0.2*sqrt(log(p[o])/n[s]))
        b.hat = coef(my.logistic.fit)
        
        ####### score vector
        W.n1 = diag(c(exp(X%*% as.matrix(b.hat)[-1,])/(1+exp(X%*% as.matrix(b.hat)[-1,]))^2)^(-1))
        zeta.try = matrix(ncol = p[o],nrow =5)
        tau.try = matrix(ncol = p[o],nrow = 5)
        
        V = matrix(ncol=n[s], nrow = p[o])
        tau = c()
        for(i in 1:p[o]){
          nodewise.try = glmnet(x= X[,-i], y = X[,i], family = "gaussian", alpha = 1, intercept = F, nlambda = 5)
          for(lambda.i in 1:5){
            V[i,] = X[,i]-X[,-i]%*%nodewise.try$beta[,lambda.i]
            zeta.try[lambda.i,i] = max(abs(as.vector(V[i,]%*%X[,-i])/sqrt(V[i,]%*%W.n1%*%V[i,])))
            tau.try[lambda.i,i] = sqrt(V[i,]%*%W.n1%*%V[i,])/(V[i,]%*% X[,i])
          }
          zeta0 = sqrt(2*log(p[o]))
          if(min(zeta.try[,i])>sqrt(2*log(p[o]))) zeta0 = tune.1*min(zeta.try[,i])
          lambda.chosen = order(nodewise.try$lambda[which(zeta.try[,i] <= zeta0)], decreasing=T)[1]
          tau[i] = tau.try[lambda.chosen,i]
          lambda.chosen = order(nodewise.try$lambda[tau.try[,i]<=tune.2[s]*tau[i]],decreasing = F)[1]
          tau[i] = tau.try[lambda.chosen,i]
          V[i,] = X[,i]-X[,-i]%*%nodewise.try$beta[,lambda.chosen]
        }
        
        
        
        V2 = V%*%W.n1
        #debaised estimator
        b.check = c()
        for(j in 1:p[o]){
          b.check[j] = b.hat[j+1]+(V2[j,]%*%(y-f(X %*% as.matrix(b.hat)[-1])))/(V[j,] %*% X[,j])
        }
        
        
        #### global test
        test.g[bb] = max((b.check/tau)^2) >= 2*log(p[o])-log(log(p[o]))+q.alpha
        
        
        #univariate screening
        test.adh[bb] = min(p.adjust(univglms(y=y,x=X)[,2],method="bonferroni"))<alpha
        
        #rescaled LLR
        if(r[s]==0.2){
          out=c()
          out0 = glm(y~X, family="binomial")$deviance
          for(BB in 1:p[o]){
            out[BB]=glm(y~X[,-BB], family="binomial")$deviance-out0
          }
          test.llr[bb] = min(p.adjust(1-pchisq(out/1.26,df=1),method="bonferroni"))<alpha
        }
        
        if(r[s]==0.4){
          out=c()
          out0 = glm(y~X, family="binomial")$deviance
          for(BB in 1:p[o]){
            out[BB]=glm(y~X[,-BB], family="binomial")$deviance-out0
          }
          test.llr[bb] = min(p.adjust(1-pchisq(out/2.05,df=1),method="bonferroni"))<alpha
        }
        
        
      }
      return(data.frame(test.g,test.adh,test.llr)) 
    }
    
    
    stopCluster(cl)
    out.g= do.call(rbind, G.end)
    
    power.g[o,s] = sum(as.logical(out.g[,1]))/BBB
    power.adh[o,s]=sum(as.logical(out.g[,2]))/BBB
    if(r[s]<0.5) power.llr[o,s] =sum(as.logical(out.g[,3]))/BBB
    
    print(c(power.g[o,s],power.adh[o,s],power.llr[o,s]))
  }
  
}

save(power.g,power.adh,power.llr, file="Global_Err_sp2.RData")



###### Powers

#####Global Testing

sp=2

#p=c(100)
p=c(100,200,300,400)
r = c(0.2,0.4,1.2)
alpha= 0.05
q.alpha = -log(pi)-2*log(log(1/(1-alpha)))
numcores=8
BBB=400
batchsize=2

tune.1=1.5
tune.2=c(1,1,1)
f = function(x){
  exp(x)/(1+exp(x))
}


power.g = matrix(rep("NA",12),ncol=3, nrow=4)
power.adh = matrix(rep("NA",12),ncol=3, nrow=4)
power.llr= matrix(rep("NA",12),ncol=3, nrow=4)

for(o in 1:length(p)){
  n=as.integer(p[o]/r)
  b = rep(0,p[o])
  b[1:sp] = rep(c(-0.75,0.75), sp/2)
  sig = toeplitz(seq(0.7,0.7,length.out = p[o]/10))
  Sig = bdiag(rep(list(sig),10))+diag(rep(0.3,p[o])) 
  
  for(s in 1:(length(r))){
    
    
    cl <- parallel::makeCluster(numcores, outfile = "debug.txt")
    clusterSetRNGStream(cl, 123)
    registerDoParallel(cl)
    test.g = rep("NA",batchsize)
    test.adh=rep("NA",batchsize)
    test.llr=rep("NA",batchsize)
    
    G.end=foreach(round=1:(BBB/batchsize)) %dopar% {
      library(MASS)
      library(purrr)
      library(glmnet)
      library(Rfast)
      for(bb in 1:batchsize){
        X = mvrnorm(n[s], mu=rep(0,p[o]), Sigma=Sig)
        
        prob = exp(X %*% b)/(1+exp(X %*% b))
        y = rep(1,n[s])
        while(sum(y)/n[s]<0.02 | sum(y)/n[s]>0.98 ){
          for(gen.y in 1:n[s]){
            y[gen.y]=rbinom(1,1,prob[gen.y])
          }
        }
        
        ####### biased estimation
        
        
        my.logistic.fit = glmnet(x = X, y = y, family = "binomial", alpha = 1,  intercept=F, lambda = 0.25*sqrt(log(p[o])/n[s]))
        b.hat = coef(my.logistic.fit)
        
        ####### score vector
        W.n1 = diag(c(exp(X%*% as.matrix(b.hat)[-1,])/(1+exp(X%*% as.matrix(b.hat)[-1,]))^2)^(-1))
        zeta.try = matrix(ncol = p[o],nrow =5)
        tau.try = matrix(ncol = p[o],nrow = 5)
        
        V = matrix(ncol=n[s], nrow = p[o])
        tau = c()
        for(i in 1:p[o]){
          nodewise.try = glmnet(x= X[,-i], y = X[,i], family = "gaussian", alpha = 1, intercept = F, nlambda = 5)
          for(lambda.i in 1:5){
            V[i,] = X[,i]-X[,-i]%*%nodewise.try$beta[,lambda.i]
            zeta.try[lambda.i,i] = max(abs(as.vector(V[i,]%*%X[,-i])/sqrt(V[i,]%*%W.n1%*%V[i,])))
            tau.try[lambda.i,i] = sqrt(V[i,]%*%W.n1%*%V[i,])/(V[i,]%*% X[,i])
          }
          zeta0 = sqrt(2*log(p[o]))
          if(min(zeta.try[,i])>sqrt(2*log(p[o]))) zeta0 = tune.1*min(zeta.try[,i])
          lambda.chosen = order(nodewise.try$lambda[which(zeta.try[,i] <= zeta0)], decreasing=T)[1]
          tau[i] = tau.try[lambda.chosen,i]
          lambda.chosen = order(nodewise.try$lambda[tau.try[,i]<=tune.2[s]*tau[i]],decreasing = F)[1]
          tau[i] = tau.try[lambda.chosen,i]
          V[i,] = X[,i]-X[,-i]%*%nodewise.try$beta[,lambda.chosen]
        }
        
        
        
        V2 = V%*%W.n1
        #debaised estimator
        b.check = c()
        for(j in 1:p[o]){
          b.check[j] = b.hat[j+1]+(V2[j,]%*%(y-f(X %*% as.matrix(b.hat)[-1])))/(V[j,] %*% X[,j])
        }
        
        
        #### global test
        test.g[bb] = max((b.check/tau)^2) >= 2*log(p[o])-log(log(p[o]))+q.alpha
        test.adh[bb] = min(p.adjust(regression(X,y)[,2],method="bonferroni"))<alpha
        
        if(r[s]==0.2){
          out=c()
          out0 = glm(y~X, family="binomial")$deviance
          for(BB in 1:p[o]){
            out[BB]=glm(y~X[,-BB], family="binomial")$deviance-out0
          }
          test.llr[bb] = min(p.adjust(1-pchisq(out/1.26,df=1),method="bonferroni"))<alpha
        }
        
        if(r[s]==0.4){
          out=c()
          out0 = glm(y~X, family="binomial")$deviance
          for(BB in 1:p[o]){
            out[BB]=glm(y~X[,-BB], family="binomial")$deviance-out0
          }
          test.llr[bb] = min(p.adjust(1-pchisq(out/2.05,df=1),method="bonferroni"))<alpha
        }
        
        
      }
      return(data.frame(test.g,test.adh,test.llr)) 
    }
    
    
    stopCluster(cl)
    out.g= do.call(rbind, G.end)
    
    power.g[o,s] = sum(as.logical(out.g[,1]))/BBB
    power.adh[o,s]=sum(as.logical(out.g[,2]))/BBB
    if(r[s]<0.5) power.llr[o,s] =sum(as.logical(out.g[,3]))/BBB
    
    print(c(power.g[o,s],power.adh[o,s],power.llr[o,s]))
  }
  
}

save(power.g,power.adh,power.llr, file="Global_Pw_sp2.RData")


#The simulations were ran in a parallel computing platform. The following codes consist of single round of simulation with specificed n, s, and p.

args= commandArgs(trailingOnly=TRUE)
set.seed(123)
library(knockoff)
library(glmnet)
library(doMC)
library(Rfast)
library(MASS)
library(purrr)


sp=50
n=800
p=800

tune.1=1.5
tune.2=1
f = function(x){
  exp(x)/(1+exp(x))
}


#Set the regression coefficients.
b = rep(0,p)
b[1:sp] = rep(c(3,-3), sp/2)


power = 0
fdr = 0
power.bh = 0
fdr.bh = 0
power.by = 0
fdr.by = 0
power.koff=0
fdr.koff=0
power.adh=0
fdr.adh=0
test.m = c()
sig1 = toeplitz(seq(0.1,0,length.out = p/10))
Sig = bdiag(rep(list(sig1),10))+diag(rep(0.9,p))
#Sig=diag(rep(1,p))
X=mvrnorm(n, mu=rep(0,p), Sigma=0.01*Sig)
while(length(which(abs(X%*% b)>3))>0){
  X[which(abs(X%*%b)>3),]=mvrnorm(length(which(abs(X%*% b)>3)), mu=rep(0,p), Sigma=0.01*Sig)
}
prob = exp(X %*% b)/(1+exp(X %*% b))
y = rep(1,n)
while(sum(y)/n<0.02 | sum(y)/n>0.98 ){
  for(gen.y in 1:n){
    y[gen.y]=rbinom(1,1,prob[gen.y])
  }
}
rm(Sig)

####### lasso estimation

my.logistic.fit = glmnet(x = X, y = y, family = "binomial", alpha = 1,  intercept=F,
                         lambda = 0.01*sqrt(log(p)/n), standardize=F)
b.hat = coef(my.logistic.fit)

####### score vector

W.n1 = c(exp(X%*% as.matrix(b.hat)[-1,])/(1+exp(X%*% as.matrix(b.hat)[-1,]))^2)^(-1)
zeta.try = matrix(ncol = p,nrow =5)
tau.try = matrix(ncol = p,nrow = 5)

V = matrix(ncol=n, nrow = p)
tau = c()
for(i in 1:p){
  nodewise.try = glmnet(x= X[,-i], y = X[,i], family = "gaussian", alpha = 1, intercept = F, nlambda = 5, standardize = F)
  for(lambda.i in 1:5){
    V[i,] = X[,i]-X[,-i]%*%nodewise.try$beta[,lambda.i]
    zeta.try[lambda.i,i] = max(abs(as.vector(V[i,]%*%X[,-i])/sqrt(sum((V[i,])^2*W.n1))))
    tau.try[lambda.i,i] = sqrt(sum((V[i,])^2*W.n1))/(V[i,]%*% X[,i])
  }
  zeta0 = sqrt(2*log(p))
  if(min(zeta.try[,i])>sqrt(2*log(p))) zeta0 = tune.1*min(zeta.try[,i])
  lambda.chosen = order(nodewise.try$lambda[which(zeta.try[,i] <= zeta0)], decreasing=T)[1]
  tau[i] = tau.try[lambda.chosen,i]
  lambda.chosen = order(nodewise.try$lambda[tau.try[,i]<=tune.2*tau[i]],decreasing = F)[1]
  tau[i] = tau.try[lambda.chosen,i]
  V[i,] = X[,i]-X[,-i]%*%nodewise.try$beta[,lambda.chosen]
}


V2 = t((t(V)*W.n1))
#debaised estimator
b.check = c()
for(j in 1:p){
  b.check[j] = b.hat[j+1]+(V2[j,]%*%(y-f(X %*% as.matrix(b.hat)[-1])))/(V[j,] %*% X[,j])
}

#### multiple test
M = b.check/tau

cutoff = function(x, alpha) p*(1-1*pchisq(x^2, df=1))/max(1,sum(abs(M) > x)) - alpha

t = sqrt(2*log(p))
x= seq(0, sqrt(2*log(p)-2*log(log(p))), 0.001)
for(k in 1:length(x)){
  if(cutoff(x[k], 0.2)<0) t = min(x[k],t)
}


test.m[abs(M)>t] = 1
test.m[abs(M)<t] = 0

power = sum(test.m[1:sp])/sp
fdr = sum(test.m[-(1:sp)])/(max(c(sum(test.m),1)))

#BH procedure
BH = (p.adjust(2-2*pnorm(abs(M)),method="BH")<.2)
power.bh =sum(BH[1:sp])/sp
fdr.bh = sum(BH[-(1:sp)])/max(c(1,sum(BH)))

#BY procedure
BY = (p.adjust(2-2*pnorm(abs(M)),method="BY")<.2)
power.by = sum(BY[1:sp])/sp
fdr.by = sum(BY[-(1:sp)])/max(c(1,sum(BY)))  

#knockoff procedure
k_stat = function(X, X_k, y) stat.glmnet_coefdiff(X, X_k, y, nlambda=200, family = "binomial")
result = knockoff.filter(X, y, statistic=k_stat, fdr=0.2)
koff=result$selected

power.koff = length(which(koff<sp+1))/sp
fdr.koff = length(which(koff >sp))/max(c(1,length(koff)))  

#univariate screening with BH
adh = (p.adjust(univglms(y=y,x=X)[,2],method="BH")<.2)
power.adh = sum(adh[1:sp])/sp
fdr.adh = sum(adh[-(1:sp)])/max(c(1,sum(adh)))


out=c(power, fdr, power.bh, fdr.bh, power.by, fdr.by, power.koff, fdr.koff, power.adh, fdr.adh)

save(out, file= args[1])


#The following codes consist of the simulations of FDV/power for a given p, which is set to be 1200 below.
args= commandArgs(trailingOnly=TRUE)
set.seed(123)
library(knockoff)
library(glmnet)
library(doMC)  ##windows下不可用，只适用于Linux，Mac OSX
library(Rfast)
library(MASS)
library(purrr)

pdv.temp = matrix(ncol=4,nrow=3)
fdv.temp = matrix(ncol=4,nrow=3)

sp=c(40,50,60)  ## k

p=c(1200)

tune.1=1.5
tune.2=c(1,1,1)
f = function(x){
  exp(x)/(1+exp(x))
}

b = rep(0,p)

for(s in 1:3){
  
  b[1:sp[s]] = rep(c(3,-3), sp[s]/2)
  
  
  sig1 = toeplitz(seq(0.1,0,length.out = p/10))
  Sig = bdiag(rep(list(sig1),10))+diag(rep(0.9,p))
  
  n=c(400,600,800,1000)  ## let n vary
  for(o in 1:100){
    X=mvrnorm(n[o], mu=rep(0,p), Sigma=0.01*Sig)
    while(length(which(abs(X%*% b)>3))>0){
      X[which(abs(X%*%b)>3),]=mvrnorm(length(which(abs(X%*% b)>3)), mu=rep(0,p), Sigma=0.01*Sig)
    }
    prob = exp(X %*% b)/(1+exp(X %*% b))
    y = rep(1,n[o])
    while(sum(y)/n[o]<0.02 | sum(y)/n[o]>0.98 ){
      for(gen.y in 1:n[o]){
        y[gen.y]=rbinom(1,1,prob[gen.y])
      }
    }
    
    
    ####### biased estimation
    
    my.logistic.fit = glmnet(x = X, y = y, family = "binomial", alpha = 1,  intercept=F,
                             lambda = 0.01*sqrt(log(p)/n[o]), standardize=F)
    b.hat = coef(my.logistic.fit)
    
    ####### score vector
    
    W.n1 = c(exp(X%*% as.matrix(b.hat)[-1,])/(1+exp(X%*% as.matrix(b.hat)[-1,]))^2)^(-1)
    zeta.try = matrix(ncol = p,nrow =5)
    tau.try = matrix(ncol = p,nrow = 5)
    
    V = matrix(ncol=n[o], nrow = p)
    tau = c()
    for(i in 1:p){
      nodewise.try = glmnet(x= X[,-i], y = X[,i], family = "gaussian", alpha = 1, intercept = F, nlambda = 5, standardize = F)
      for(lambda.i in 1:5){
        V[i,] = X[,i]-X[,-i]%*%nodewise.try$beta[,lambda.i]
        zeta.try[lambda.i,i] = max(abs(as.vector(V[i,]%*%X[,-i])/sqrt(sum((V[i,])^2*W.n1))))
        tau.try[lambda.i,i] = sqrt(sum((V[i,])^2*W.n1))/(V[i,]%*% X[,i])
      }
      zeta0 = sqrt(2*log(p))
      if(min(zeta.try[,i])>sqrt(2*log(p))) zeta0 = tune.1*min(zeta.try[,i])
      lambda.chosen = order(nodewise.try$lambda[which(zeta.try[,i] <= zeta0)], decreasing=T)[1]
      tau[i] = tau.try[lambda.chosen,i]
      lambda.chosen = order(nodewise.try$lambda[tau.try[,i]<=tune.2*tau[i]],decreasing = F)[1]
      tau[i] = tau.try[lambda.chosen,i]
      V[i,] = X[,i]-X[,-i]%*%nodewise.try$beta[,lambda.chosen]
    }
    
    
    
    
    V2 = t((t(V)*W.n1))
    #debaised estimator
    b.check = c()
    for(j in 1:p){
      b.check[j] = b.hat[j+1]+(V2[j,]%*%(y-f(X %*% as.matrix(b.hat)[-1])))/(V[j,] %*% X[,j])
    }
    
    #### multiple test
    M = b.check/tau
    
    t.fdv = qnorm((2-10/p)/2)
    
    pdv.temp[s,o] = sum(abs(M[1:sp[s]])>t.fdv)/sp[s]
    fdv.temp[s,o] = sum(abs(M[-(1:sp[s])])>t.fdv)
    
  }
}
out=c(pdv.temp,fdv.temp)
save(out, file= args[1])


