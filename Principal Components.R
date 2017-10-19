setwd('Y:\\My Documents\\Presentations\\2013 RPM')
.libPaths('T:\\GLM\\Reference\\R\\64_Packages')

X<-read.table('data01.txt',col.names=c('CovA','sqft'))

X[1:5,]
eigen(cor(X))$vectors
princomp(X,cor=TRUE,scores=TRUE)$loadings

cor(X)

eigen(cor(X))$values
(princomp(X,cor=TRUE,scores=TRUE)$sdev)^2

eigen(cor(X))$values[1] / sum(eigen(cor(X))$values)

min(scale(X)[,1]);max(scale(X)[,1])
min(scale(X)[,2]);max(scale(X)[,2])
min(princomp(X,cor=TRUE,scores=TRUE)$scores[,1]);max(princomp(X,cor=TRUE,scores=TRUE)$scores[,1])
min(princomp(X,cor=TRUE,scores=TRUE)$scores[,2]);max(princomp(X,cor=TRUE,scores=TRUE)$scores[,2])

princomp(X,cor=TRUE,scores=TRUE)$scores[1:5,]
(scale(X)%*%eigen(cor(X))$vectors)[1:5,]

win.graph(width=5.5,height=5.5,pointsize=8)
plot(x=X[,1],y=X[,2],
     #xlim=c(-3,9),ylim=c(-4,8),
     xlab='Coverage A',ylab='Square Feet')


win.graph(width=5.5,height=5.5,pointsize=8)
plot(x=scale(X)[,1],y=scale(X)[,2],
     type='p',
     xlim=c(-3,9),ylim=c(-4,8),
     xlab='Coverage A',ylab='Square Feet')
points(scale(X)%*%eigen(cor(X))$vectors,col='red')
abline(a=0,b=1,col='red')
abline(a=0,b=-1,col='red')

# Plot of CovA vs Cov C
library('graphics')

win.graph(width=5.5,height=5.5,pointsize=8)
plot(x=X[,1],y=X[,1]*0.5,
     xlab='Coverage A',ylab='Coverage C',
     xaxt='n',yaxt='n')
xpos<-c(500000,1000000,1500000)
axis(side=1,at=xpos,labels=sprintf("%d",xpos/1000))  # bottom axis
axis(side=2,at=xpos*.5,labels=sprintf("%d",xpos*.5/1000))  # left axis
abline(a=0,b=1/2,col='red')

sprintf("precision %.*f, width '%*.3f'", 3, pi, 8, pi)
n <- 1:20
sprintf(paste("e with %2d digits = %.",n,"g",sep=""), n, exp(1))
