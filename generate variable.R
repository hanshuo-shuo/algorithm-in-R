#generate variable ex
gdata=function(n,b0,b1){
  ##n:sample size
  ##bo,b1:parament
  x=rnorm(n)
  pi=exp(b0+b1*x)/(1+exp(b0+b1*x))##生成储存所有pi值得向量
  y=rep(0,n)
  u=runif(n)
  for(i in 1:n){
    if(u[i]<pi[i]) y[i]=1
  }
  
  return(data.frame(x,y))
}
main=function(b0,b1){
  return(gdata(1000,b0,b1))
}
data1=main(-0.5,0.5)
hist(data1$y)
lines(density(data1$y))
