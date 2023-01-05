#1D Brownian Motion
BM<- matrix(NA, nrow=500, ncol=3)

#specifying seed
set.seed(8221056)

#simulating trajectories
for (j in 1:3) {
BM[1,j]<- 0

for (i in 2:500) 
  BM[i,j]<- BM[i-1,j] + sqrt(0.01)*rnorm(1)
}

#plotting trajectories

matplot(BM, type="l", main="Brownian Motion", lty=1, lwd=2, col=2:4, 
ylim=c(range(BM)), xlab="Time", ylab="Position", panel.first=grid())




