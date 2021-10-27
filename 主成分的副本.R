set.seed(1)
x1<-round(runif(10,1,3),digits = 1)
set.seed(2)
x2<-round(runif(10,2,4),digits = 1)
set.seed(3)
e<-round(runif(10,-0.3,0.3),digits = 1)
x3<-x1+e 
set.seed(4)
x4<-round(runif(10,4,6),digits = 1)


set.seed(1)
r<-rnorm(10)
y<-15+3*x1+2*x2+4*x3-3*x4+r

test<-data.frame(y,x1,x2,x3,x4)
lm.sol<-lm(y~x1+x2+x3+x4, data=test);
summary(lm.sol);
test.pr<-princomp(~x1+x2+x3+x4, data=test, cor=T);
summary(test.pr, loadings=TRUE);

pre<-predict(test.pr);
test$z1<-pre[,1];
test$z2<-pre[,2];
test$z3<-pre[,3];

lm.sol<-lm(y~z1+z2+z3, data=test);
summary(lm.sol);

beta<-coef(lm.sol); A<-loadings(test.pr);
x.bar<-test.pr$center; x.sd<-test.pr$scale;
coef<-(beta[2]*A[,1]+ beta[3]*A[,2])/x.sd;
beta0 <- beta[1]- sum(x.bar * coef);
c(beta0, coef);

