## Q x
## x1 and x3 has strong linear relation
set.seed(2)
x1<-round(runif(10,1,3),digits = 1)
set.seed(2)
x2<-round(runif(10,2,4),digits = 1)
set.seed(2)
e<-round(runif(10,-0.3,0.3),digits = 1)
x3<-x1+e 
set.seed(2)
x4<-round(runif(10,4,6),digits = 1)


set.seed(1)
r<-rnorm(10)
y<-13+3*x1+4*x2+3*x3-2*x4+r
lm.sol <- lm(Y ~ ., data = cement)
summary(lm.sol)
library(MASS)
ridge.sol <- lm.ridge(Y ~ ., lambda = seq(0, 150, length = 151), data = cement, 
                      model = TRUE)
names(ridge.sol) 
ridge.sol$lambda[which.min(ridge.sol$GCV)]
ridge.sol$coef[which.min(ridge.sol$GCV)] 
par(mfrow = c(1, 2))
matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression(lamdba), ylab = "Cofficients", 
        type = "l", lty = 1:20)
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])
library(ridge)
mod <- linearRidge(Y ~ ., data = cement)
summary(mod)

