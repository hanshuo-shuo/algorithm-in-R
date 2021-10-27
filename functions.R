##
directpoly=function(x,cc){
  #x=[x^n-1,x^n-2,....,x,1]  variable vector
  #cc=[cn,cn-1,....,c2,c1] coefficient vector
  ##output:poly function value
  len.c=length(cc)
  sum=0
  for( i in 1:len.c){
    sum=sum+cc[i]*x^(i-1)
    
  } 
  return(sum)
}
y=rnorm(100000)
coeff=c(2,-6,0,1.2,1,2,3,9)
result=directpoly(y,coeff)
result
##
hornerpoly=function(x,cc){
  n=length(cc)
  aa=cc[n]
  i=n
  while (i>0){
    aa=aa*x+cc[i-1]
    i=i-1
  }  
  return(aa)
    
  
}

result=hornerpoly(y,coeff)
result
##compare the efficency
system.time(hornerpoly(y,coeff))
system.time(directpoly(y,coeff))
########


##############
###question 2
my_function=function(y){
  ##input y is a vector
  m=mean(y)
  s=sd(y)
  n=length(y)
  skew=mean((y-m)^3/s^3)  ###Æ«¶È
  kurt=sum((y-m)^4/s^4)/n-3   ###·å¶È
  return(c(mean=m,stdev=s,skew=skew,kurtosis=kurt))
}
y=rnorm(10)
my_function(y)



#######
#Question 3
R_fun1=function(vec){
  ##vec:a vector
  if (!is.numeric(vec))print('vectors must be numbers')
  else {
    vec=vec[!is.na(vec)]
    result=mean(((vec-mean(vec))/sd(vec))^3)
    if((abs(result)<1))
      return(c(skewness=result,descstats=c(standard_deviation=sd(vec),mean=mean(vec))))
    
    else
      return(c(skewness=result,descstats=summary(vec)))
  }
  
}
result1=R_fun1(c("Arthor","Mary","Robber"))
result2=R_fun1(rnorm(100))
result3=R_fun1(rexp(100,5))
