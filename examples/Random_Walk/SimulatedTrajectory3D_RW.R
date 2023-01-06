# This build of rgl does not include OpenGL functions.  Use
#  rglwidget() to display results, e.g. via options(rgl.printRglwidget = TRUE).
options(rgl.printRglwidget = TRUE)

#specifying number of steps
nsteps<- 5000

#specifying seed
set.seed(830126)

#defining walk as matrix
walk<- matrix(NA, nrow=nsteps, ncol=3)

#setting starting point
walk[1,]<- c(0,0,0)

#defining random steps
rstep<- matrix(c(1, 0, 0,-1, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 1, 0, 0, -1), nrow=6, 
ncol=3, byrow=TRUE)

#simulating trajectories
for (i in 2:nsteps) 
walk[i,]<- walk[i-1,]+rstep[sample(1:6, size=1),]

#plotting trajectories
library(plot3D)

library(rgl)
lines3D(walk[,1], walk[,2], walk[,3], main="Three-dimensional
Random Walk", col="blue", xlim=range(walk[,1]), 
ylim=range(walk[,2]), zlim=range(walk[,3]), 
xaxt="n", yaxt="n", zaxt="n")

#adding starting point
points3D(x=walk[1,1], y=walk[1,2], z=walk[1,3], add=TRUE, pch=16, 
col="green", cex=2)

#adding ending point
points3D(walk[nsteps,1], walk[nsteps,2], walk[nsteps,3], add=TRUE, pch=16, 
col="red", cex=2)


