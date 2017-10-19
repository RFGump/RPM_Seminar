setwd('Y:\\My Documents\\Presentations\\2013 RPM')
.libPaths('T:\\GLM\\Reference\\R\\64_Packages')

X<-read.table('Census.txt',col.names=c('Pop','Emp','Inc','Home','HEmp','Col'))
R<-cor(X);round(R,2)
round(eigen(R)$values,3)
round(eigen(R)$vectors,3)

win.graph(width=4,height=5.5,pointsize=8)
plot(eigen(R)$values,type='b',ylab='eigenvalues',xlab='')

eigen(R)$values
cumsum(eigen(R)$values)
eigen(R)$values/sum(eigen(R)$values)
round(cumsum(eigen(R)$values)/sum(eigen(R)$values),3)

princomp(X,cor=TRUE)$loadings

(princomp(X,cor=TRUE,scores=TRUE)$sdev)^2
