
test<-data.frame(y=c(8.4,9.6,10.4,11.4,12.2,14.2,15.8,17.9,19.6,20.8),
                 x1=c(82.9,88,99.9,105.3,117.7,131,148.2,161.8,174.2,184.7),
                 x2=c(92,93,96,94,100,101,105,112,112,112),
                 x3=c(17,21.3,25.1,29,34,40,44,49,51,53),
                 x4=c(94,96,97,97,100,101,104,109,111,111))
lm.sol<-lm(y~x1+x2+x3+x4, data=test);
summary(lm.sol);
test.pr<-princomp(~x1+x2+x3+x4, data=test, cor=T);
summary(test.pr, loadings=TRUE);

pre<-predict(test.pr);
test$z1<-pre[,1];
test$z2<-pre[,2];


lm.sol<-lm(y~z1+z2, data=test);
summary(lm.sol);

beta<-coef(lm.sol); A<-loadings(test.pr);
x.bar<-test.pr$center; x.sd<-test.pr$scale;
coef<-(beta[2]*A[,1]+ beta[3]*A[,2])/x.sd;
beta0 <- beta[1]- sum(x.bar * coef);
c(beta0, coef);

