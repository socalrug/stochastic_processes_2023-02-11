#specifying parameters
lambda <- 0.3
mu <- 0.1
njumps <- 20

#setting state and time as vectors
N <- c()
time <- c()

#setting initial values
N[1] <- 1
time[1] <- 0

#specifying seed
set.seed(1022171)

#simulating trajectory
i <- 2

repeat {
  time.birth <- (-1/(N[i-1]*lambda))*log(1-runif(1))
  time.death <- (-1/(N[i-1]*mu))*log(1-runif(1))

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
plot(time, N, type="l", main="Linear Birth-and-death
     Process", lty=1, lwd=2, col="purple", xlab="Time", ylab="State",
     panel.first=grid())
