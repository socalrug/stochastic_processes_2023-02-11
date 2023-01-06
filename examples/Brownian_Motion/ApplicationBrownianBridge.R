#defining Brownian motion and Brownian bridge as matrices
BM<- matrix(NA, nrow=480, ncol=1000)
BB<- matrix(NA, nrow=480, ncol=1000)

#specifying seed
set.seed(6769712)

#simulating trajectories of Brownian motion
for (j in 1:1000) {
BM[1,j]<- 0

for (i in 2:480) 
BM[i,j]<- BM[i-1,j] + rnorm(1)
}

#computing trajectories of Brownian bridge
for(j in 1:1000){
  for (i in 1:480)
BB[i,j]<- BM[i,j]-i/480*BM[480,j]
}

#computing ranges
range<- c()
for(j in 1:1000) 
  range[j]<- max(BB[,j])-min(BB[,j])

#computing sample diameter of home range
print(diameter<- mean(range))


