#2D Brownian Motion
BM<- matrix(NA, nrow=5000, ncol=2)

#specifying seed
set.seed(34885002)

#simulating two independent Brownian motions
for (j in 1:2) {
  BM[1,j]<- 0
  
  for (i in 2:5000) 
    BM[i,j]<- BM[i-1,j] + sqrt(0.01)*rnorm(1)
}

#plotting trajectory
plot(x=BM[,1], y=BM[,2], main="Two-dimensional Brownian Motion",
type="l", col=4, xlab="x", ylab="y", xlim=range(BM[,1]),
ylim=range(BM[,2]), panel.first=grid())

#adding starting point
points(cbind(BM[1,1], BM[1,2]), pch=16, cex=2, col="green")

#adding ending point
points(cbind(BM[5000,1],BM[5000,2]), pch=16, cex=2, col="red")