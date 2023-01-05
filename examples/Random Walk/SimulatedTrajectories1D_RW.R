#specifying parameters
p<- 0.6
nsteps<- 25
ntraj<- 3

#specifying seed
set.seed(45568223)

#defining walk as matrix
walk<- matrix(NA, nrow=nsteps, ncol=ntraj)

#simulating trajectories
for (j in 1:ntraj) {
walk[1,j]<- 0
for (i in 2:nsteps) 
walk[i,j]<- ifelse(runif(1)<p, walk[i-1,j]+1, walk[i-1,j]-1)
}

#plotting trajectories
matplot(walk, type="l", main="One-dimensional Random Walk",
        lty=1, lwd=2, col=2:4, ylim=c(range(walk)), xlab="Step", 
ylab="Position", panel.first=grid())

points(1:nsteps, walk[,1], pch=16, col=2)
points(1:nsteps, walk[,2], pch=16, col=3)
points(1:nsteps, walk[,3], pch=16, col=4)