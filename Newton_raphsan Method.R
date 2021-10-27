###Newton_raphsan Method
## given po around the real root
## update pi by the fomular:pn=pn-1 - f(pn-1)/f`(pn-1)
try1=function(x){
  f=x
  J=((x^2-1)/3-x)/(2*x/3-1)
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
  if(iter<N0) return(x)
  else print("error")
}                                               
Newtons_methods(-1,1,0.0001,1000)
