#specifying parameters
lambda <- 2
njumps <- 20

#defining states
N <- 0:njumps

#setting time as vector
time <- c()

#setting initial value for time
time[1] <- 0

#specifying seed
set.seed(333422)

#simulating trajectory
for (i in 2:(njumps+1)) {
  time[i] <- time[i-1]+round((-1/lambda)*log(runif(1)),2)
}

#plotting trajectory
plot(time, N, type="n", main="Simulated Poisson Process", xlab="Time", ylab="State",
     panel.first = grid())

segments(time[-length(time)], N[-length(time)], time[-1]-0.07, N[-length(time)],
         lwd=2, col="blue")

points(time, N, pch=20, col="blue")
points(time[-1], N[-length(time)], pch=1, col="blue")
