##(1)generating data
gdata=function(beta,n,p){
##beta:vector paraments; n:sample size;p:probability
  xi1=rnorm(n)
  xi2=rbinom(n,1,p)
  tem=beta[1]+beta[2]*xi1+beta[3]*xi2
  pi=exp(tem)/(1+exp(tem))##生成储存所有pi值得向量
  y=rep(0,n)
  u=runif(n)
  y[u<=pi]=0 
  return(list(x1=xi1,x2=xi2,y=y))
}
gdata(c(1,-log(2),log(3)),100,0.5)

#(2)maximum likehood estimator and evaluate the performance
MLE=function(begin,data){ 
  #data: data generating from the (1)
  #begin   :the beginning of the iteration
  #return a list of gradient and hession matrix
  beta0=begin[1]
  beta1=begin[2]
  beta2=begin[3]
  x1=data$x1
  x2=data$x2
  y=data$y
  temp=exp(beta0+beta1*x1+beta2*x2)
  pi=1-1/(1+temp)
  pi2=temp/((1+temp)^2)
  derivb01=sum(y-pi)
  derivb11=sum(y*x1-pi*x1)
  derivb21=sum(y*x2-pi*x2)
  gradient=c(derivb01,derivb11,derivb21)
  h1=c(-sum(pi2),-sum(pi2*x1),-sum(pi2*x2))
  h2=c(-sum(pi2*x1),-sum(pi2*x1*x1),-sum(pi2*x2*x1))
  h3=c(-sum(pi2*x2),-sum(pi2*x2*x1),-sum(pi2*x2*x2))
  hessian=rbind(h1,h2,h3)
  return(list(gradient=gradient,hessian=hessian))
}

Judge=function(begin,data){
  #check whether the hession matrix is inversible
  #newton-raphson iteration
  M=MLE(begin,data)
  hessian=M$hessian
  gradient=M$gradient
  hessian.ind=0                                  
  if(rcond(hessian)>1e-10)         
  {                                              
    est=begin-solve(hessian,gradient)
  }
  else
  {
    est=begin                              
    hessian.ind=1                                  
  }
  return(list(estimator=est,hessian.ind=hessian.ind))
}

Iteration=function(begin,data,N=100000,tol=1e-5){
  iter=1
  dif=1
  flag=1
  for (iter in 1:N)
  { 
    M=Judge(begin,data)
    para=M$est
    hessian.ind=M$hessian.ind
    if(hessian.ind==0)
    {
      dif=sum(abs(para-begin))
      if(dif<=tol) {flag=0;break}              
    }
    if(hessian.ind==1) {break}
    begin=para
  }
  return(c(begin,hessian.ind,flag))
}

simulation=function(begin,beta,n,p,N=100000,tol=1e-5,Nsim=5000){
  #beta:the parameters
  #n:sample size
  #p:possibility
  m=length(beta)
  est=matrix(rep(0,Nsim*(m+2)),Nsim,m+2)
  for (i in 1:Nsim)
  { 
    data=gdata(beta,n,p)
    est[i,]=Iteration(begin,data)
  }
  est=est[est[,m+1]==0&est[,m+2]==0,1:m]           
  est=round(est,3)
  T1=apply(est,2,mean)
  bias=T1-beta
  SD=apply(est,2,sd)
  TNsim=nrow(est)                                 
  cp=matrix(rep(0,TNsim*m),m,TNsim)                
  cp[beta>=t(est)-1.96*SD & beta<=t(est)+1.96*SD]=1      
  CP=apply(cp,1,mean)
  MSE=SD^2+bias^2
  bias=round(bias,3)
  SD=round(SD,3)
  MSE=round(SD,3)
  CP=round(CP,3)
  return(list(beta=beta,mean=T1,bias=bias,SD=SD,MSE=MSE,CP=CP,TNsim=TNsim))
}

set.seed(281)
result=simulation(c(0,0,0),c(0,log(2),log(3)),100,0.5)
result
#so the performance of maximum likehood estimator is good