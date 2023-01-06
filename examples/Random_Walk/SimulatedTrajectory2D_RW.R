#specifying number of steps
nsteps<- 10000

#specifying seed
set.seed(607335)
  
#defining walk as matrix
walk<- matrix(NA, nrow=nsteps, ncol=2)

#setting starting point
walk[1,]<- c(0,0)

#defining random steps
rstep<- matrix(c(1, 0, -1, 0, 0, 1, 0, -1), 
nrow=4, ncol=2, byrow=TRUE)

#simulating trajectories
for (i in 2:nsteps) 
walk[i,]<- walk[i-1,] + rstep[sample(1:4, size=1),]

#plotting trajectories
plot(x=walk[,1],y=walk[,2], main="Two-dimensional Random Walk",
type="l", col="blue", xlim=range(walk[,1]), 
ylim=range(walk[,2]), xlab="x", ylab="y", panel.first=grid())

#adding starting point
points(cbind(walk[1,1], walk[1,2]), pch=16, col="green", cex=2)

#adding ending point
points(cbind(walk[nsteps,1],walk[nsteps,2]), pch=16, col="red", cex=2)

