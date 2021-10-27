##Ex1逆变化
g1_data=function(a,b,n){
  ##a,b:paraments and a should < b
  ##n:sample size
  u=runif(n)
  x=rep(0,n)
  x[u<=1/2]=2*a+sqrt(2*u[u<=1/2])*(b-a)   
  x[u>1/2]=2*b-(b-a)*sqrt(2*(1-u)[u>1/2])
  return(x)
}
a=g1_data(1,2,10)

##Ex1 合成法
g2_data=function(a,b,n)
{
  u1=runif(n)
  u2=runif(n)
  x=rep(0,n)
  x[u1<=1/2]=2*a+(b-a)*sqrt(u2)[u1<=1/2]
  x[u1>1/2]=2*b-(b-a)*sqrt(u2)[u1>1/2]
  return(x)  
}


##ex2逆变化
g21_data=function(n){
  ##n:sample size
  u=runif(n)
  x=rep(0,n)
  x=(2*u[u>=1/2]-1)^(1/3)    
  x=-((2*(-u[u<1/2])+1)^(1/3))   
  return(x)
}

##合成法
g22_data=function(n){
  ##n:sample size
  u1=runif(n)
  u2=runif(n)
  x=rep(0,n)
  x[u1<=1/2]=-(1-u2[u1<=1/2])^(1/3)
  x[u1>1/2]=u2[u1>1/2]^(1/3)
  return(x)
}
g23_data=function(n){
##n :simole size
  iter=1
  result=rep(0,n)
  while (iter<=n) {
    u=runif(1)
    y=sqrt(u)
    if(u>=1/2) {result[iter]=y;iter=iter+1}
  }
  return(result)
}
###比较
a1=g21_data(1000)
a2=g22_data(1000)
a3=g23_data(1000)
sd(a1)
sd(a2)
sd(a3)
