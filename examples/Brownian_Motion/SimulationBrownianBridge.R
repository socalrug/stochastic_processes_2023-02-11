#defining Brownian motion and Brownian bridge as matrices
BM <- matrix(NA, nrow=500, ncol=3)
BB <- matrix(NA, nrow=500, ncol=3)

#specifying seed
set.seed(76435567)

#simulating trajectories of Brownian motion
for (j in 1:3) {
  BM[1,j] <- 0

  for (i in 2:500) {
    BM[i,j] <- BM[i-1,j] + sqrt(0.01)*rnorm(1)
  }
}

#computing trajectories of Brownian bridge
for(j in 1:3){
  for (i in 1:500) {
    BB[i,j] <- BM[i,j]-i/500*BM[500,j]
  }
}

#plotting trajectories of Brownian bridge
matplot(BB, type="l", main="Brownian Bridge", lty=1, lwd=2, col=2:4,
        ylim=c(range(BB)), xlab="Time", ylab="Position", panel.first=grid())

