##EX1
x1=rep(c(3,2,1),c(3,4,5))
x1

##Ex2
A=matrix(1:16,4,4,byrow=FALSE)
B=matrix(1:16,4,4,byrow=TRUE)

C=A+B
C

###内积
x3=t(A)%*%B
X4=crossprod(A,B)

###外
A%o%B
outer(A,B,"*")

##去掉行列
A1=A[-3,]
B1=B[,-3]

###内积
x3=t(A1)%*%B1
X4=crossprod(A1,B1)

###外
A1%o%B1
outer(A1,B1)
