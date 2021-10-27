##ex1
hours<-c(3,5,7,18,43,85,91,98,100,130,230,487)
nonparboot.logmean<-function(data,B){
  ## nonparametric bootstrap and data must be a vector
  ## return B bootstrap estimate of logmean
  if(is.vector(data)==F) print('input data in vector')
  else {
    n<-length(data)
    bootlogmean<-numeric(B)
    for(i in 1:B){
      sample=sample(n,replace = T)
      bootdata=data[sample]
      bootlogmean[i]=log(mean(bootdata))
    }
    return(bootlogmean)
  }
}

drive.bootstrap<-function(data,bootfunc,B){
  ## bootfuc: generating bootstrap sample
  ## return ESE,BIAS,estimate,confidence interval
  boots=bootfunc(data,B)
  ESE=sd(boots)
  logmeanhead<-log(mean(hours))
  bias=mean(boots)-logmeanhead
  bias_corrected_estimate=2*logmeanhead-mean(boots)
  confidence_interval_bypercentile=quantile(boots,c(0.025,0.975))
  confidence_interval_by_bias_correctedpercentile=quantile(2*logmeanhead-boots,c(0.025,0.975))
  return(list(ESE=ESE,bias=bias,bias_corrected_estimate=bias_corrected_estimate,
              confidence_interval_bypercentile=confidence_interval_bypercentile,confidence_interval_by_bias_correctedpercentile=confidence_interval_by_bias_correctedpercentile))
}


drive.bootstrap(hours,bootfunc = nonparboot.logmean,B=200)


## ex2
nonparboot.bino=function(data,B){
  ## data must be a vector,and shows our answer of the trials,1 is success,0 is not
  ## return B bootstrap estimate of mean
  if(is.vector(data)==F) print('input data in vector')
  else {
    bootmean=numeric(B)
    n=length(data)
    for(i in 1:B){
      index=sample(n,replace = T)
      bootsample=data[index]
      bootmean[i]=mean(bootsample)
    }
  return(bootmean)
  }
}

drive.bootstrap_bino=function(data,bootfunc,B){
  ## bootfuc: generating bootstrap sample
  meanhead=mean(data)
  boots=bootfunc(data,B)
  confidence_interval_bypercentile=quantile(boots,c(0.025,0.975))
  confidence_interval_by_bias_correctedpercentile=quantile(2*meanhead-boots,c(0.025,0.975))
  return(list(confidence_interval_bypercentile=confidence_interval_bypercentile,confidence_interval_by_bias_correctedpercentile=confidence_interval_by_bias_correctedpercentile))
}


bindata=rep(0,100)
drive.bootstrap_bino(bindata,nonparboot.bino,B=500)

##conclusion: there will be exception when apply this me#thood; for example when 
##all the data we apply is all zero the paraments we get is also all zero
