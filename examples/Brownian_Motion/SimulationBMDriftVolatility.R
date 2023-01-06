#simulating Brownian motion with drift and volatility
#specifying parameters
mu<- 1.3
sigma<- 0.5

#defining Brownian motion as matrix
BM<- matrix(NA, nrow=500, ncol=3)

#specifying seed
set.seed(8463302)

#simulating  trajectories
for (j in 1:3) {
BM[1,j]<- 0

for (i in 2:500) 
  BM[i,j]<- mu*0.01+BM[i-1,j] + sigma*sqrt(0.01)*rnorm(1)
}

#plotting trajectories
matplot(BM, type="l", main="Brownian motion with drift 
and volatility", lty=1, lwd=2, col=2:4, ylim=c(range(BM)), 
xlab="Time", ylab="Position", panel.first=grid())

