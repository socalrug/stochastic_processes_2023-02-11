#specifying parameters
lambda <- 1
mu <- 1.5
njumps <- 50000

#defining state and time as vectors
N <- c()
time <- c()

#setting initial values
N[1] <- 1
time[1] <- 0

#specifying seed
set.seed(353332)

#simulating trajectory
i <- 2

repeat  {
  time.birth <- -1/lambda*log(1-runif(1))
  time.death <- -1/mu*log(1-runif(1))

  if(time.birth < time.death | N[i-1]==0) {
    time[i] <- time[i-1] + time.birth - 0.001
  N[i] <- N[i-1]

if(i==2*njumps+2) break
else {
  time[i+1] <- time[i] + 0.001
  N[i+1] <- N[i] + 1
  i <- i+2
     }
  }

  if(time.death < time.birth & N[i-1]!=0) {
    time[i] <- time[i-1] + time.death - 0.001
    N[i] <- N[i-1]

    if(i==2*njumps+2) break
    else {
      time[i+1] <- time[i] + 0.001
      N[i+1] <- N[i] - 1
      i <- i+2
    }
  }
}

#plotting trajectory
plot(time, N, type="l", main="M/M/1 Queue Trajectory", lty=1, lwd=2, col="blue",
     xlab="Time", ylab="State", panel.first=grid())

#rerun with njumps <- 50000
print(mean(N))
