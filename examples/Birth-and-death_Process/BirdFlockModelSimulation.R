#specifying parameters
lambda <- 1.4
mu <- 1.2
alpha <- 0.3
beta <- 0.1

#defining state and time as vectors
N <- c()
time <- c()

#setting initial values
N[1] <- 10
time[1] <- 0

#specifying seed
set.seed(93743765)

#simulating trajectory
i <- 2

repeat  {

  time.birth<- (-1/(N[i-1]*lambda+alpha))*log(1-runif(1))
  time.death<- (-1/(N[i-1]*mu+beta))*log(1-runif(1))

  if(time.birth < time.death | N[i-1]==0) {
    time[i] <- time[i-1] + time.birth - 0.001
    N[i] <- N[i-1]

if(N[i]==25) break
else {

  time[i+1] <- time[i] + 0.001
  N[i+1]<- N[i] + 1
  i <- i+2
     }
  }

  if(time.death < time.birth & N[i-1]!=0) {
    time[i] <- time[i-1] + time.death - 0.001
     N[i] <- N[i-1]

    if(N[i]==25) break
    else {

      time[i+1] <- time[i] + 0.001
      N[i+1] <- N[i] - 1
      i <- i+2
    }
  }
}

#plotting trajectory
plot(time, N, type="l", main="Bird Flock Model",
     lty=1, lwd=2, col="blue", xlab="Time", ylab="Count",
     panel.first=grid())
