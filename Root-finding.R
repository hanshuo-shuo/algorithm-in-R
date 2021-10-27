
###uniroot
f=function(x){
  return(x^3-x-1)
}
uniroot(f,c(1,2))



###Newton_raphsan Method
## given po around the real root
## update pi by the fomular:pn=pn-1 - f(pn-1)/f`(pn-1)
try1=function(x){
  f=x
  J=(x^3-x-1)/(3*x^2-1)
  return(list(f=f,J=J))
}

Newtons_methods=function(a,b,tol,N0){
  iter=1
  dif=1
  x=(a+b)/2
  while(iter<=N0){
    dif=abs(try1(x)$J)
    x=x-try1(x)$J
    if(dif<tol){break}
    iter=iter+1
  }
  if(iter<N0) return(list(x=x,iter=iter))
  else print("error")
}         
Newtons_methods(1,2,1e-5,1e+5)

try2=function(x){
  f=x^3-x-1
  J=(x^3-x-1)/(3*x^2-1)
  return(list(f=f,J=J))
}

bisc=function(a,b,tol,N0){
  ##a,b parament
  ##tol:accurence 
  ##No: max number of itering
  iter=1
  dif=1
  low=a;high=b
  while(iter<=N0&dif>=tol){
    mid=(low+high)/2;temp=try2(mid)$f  ##可以直接调用函数中的变量
    if(try2(low)$f*temp<0) high=mid else low=mid
    iter=iter+1
    dif=abs(high-low)
  }
  if(iter<N0) return(list(x=mid,iter=iter))
  else print("error")
}  
bisc(1,2,1e-5,1e+5)

try3=function(x) return((x+1)^(1/3))
##定点迭代
fixpoint=function(x0,tol,N0){
  iter=1
  dif=1
  oldp=x0
  flag=0
  while (iter<=N0 & dif>tol) {
    newp=try3(oldp)
    iter=iter+1
    dif=abs(newp-oldp)
    oldp=newp
  }
  if(iter<N0) return(list(x=newp,iter=iter))
  else print('error')
}
fixpoint(1,1e-5,1e+5)
###
Scant_methods=function(a,b,tol,N0){
  iter=1
  oldp0=a
  oldp1=b
  dif=1
  while(iter<=N0&dif>tol){
    newp=oldp1-try2(oldp1)$f*(oldp1-oldp0)/(try2(oldp1)$f-try2(oldp0)$f)
    iter=iter+1
    dif=abs(newp-oldp1)
    oldp0=oldp1
    oldp1=newp
  }
  if(iter<N0) return(list(x=newp,iter=iter))
  else print("error")
}         
Scant_methods(1,1.5,1e-5,1e+5)

t1=system.time(uniroot(f,c(1,2)))
t2=system.time(Newtons_methods(1,2,1e-5,1e+5))
t3=system.time(bisc(1,2,1e-5,1e+5))
t4=system.time(fixpoint(1,1e-5,1e+5))
t1=system.time(Scant_methods(1,2,1e-5,1e+5))

