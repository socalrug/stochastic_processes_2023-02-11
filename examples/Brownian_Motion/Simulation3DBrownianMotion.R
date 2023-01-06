#3D Brownian Motion
nsteps<- 2000
BM<- matrix(NA, nrow=nsteps, ncol=3)

#specifying seed
set.seed(1133205)

#simulating three independent Brownian motions
for (j in 1:3) {
  BM[1,j]<- 0

  for (i in 2:nsteps) 
    BM[i,j]<- BM[i-1,j] + sqrt(0.01)*rnorm(1)
}

#plotting trajectory

library(plot3D)

lines3D(BM[,1], BM[,2], BM[,3], main="Three-dimensional Brownian Motion",
col="navy", xaxt="n", yaxt="n", zaxt="n",
xlim=range(BM[,1]), ylim=range(BM[,2]), zlim=range(BM[,3]))

#adding starting point
points3D(x=BM[1,1], y=BM[1,2], z=BM[1,3], add=TRUE, pch=16, cex=2, col="green")

#adding ending point
points3D(BM[nsteps,1], BM[nsteps,2], BM[nsteps,3], add=TRUE, pch=16, cex=2,
col="red")







