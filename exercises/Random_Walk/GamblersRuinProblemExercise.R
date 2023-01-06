#specifying numeric values for parameters
p <- 0.52
i <- 10
N <- 24
ntraj <- 1000000

#defining walk as vector
walk <- c()

#setting counters
nzeros <- 0
nNs <- 0
ngames <- 0

#setting seed number
set.seed(93340022)

#simulating trajectories until hitting N or 0
for (j in 1:ntraj) {
  walk[1] <- i
  k <- 2
  repeat {
    walk[k] <- ifelse(runif(1)<p, walk[k-1]+1, walk[k-1]-1)
    ngames <- ngames + 1

    if (walk[k]==N) {
        break
    } else if(walk[k]==0) {
      nzeros <- nzeros+1
      break
    }
    k <- k+1
  }
}

print(prob.zeros  <- nzeros/ntraj)
print(mean.ngames <- ngames/ntraj)
