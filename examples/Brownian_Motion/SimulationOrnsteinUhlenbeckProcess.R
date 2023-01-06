#specifying parameters
theta <- 0.8
mu <- 1.6
sigma <- 0.5

#specifying seed
set.seed(2043442)

#defining Ornstein-Uhlenbeck trajectory as vector
OU <- c()

#specifying initial value
OU[1] <- 2

#simulating trajectory
for (i in 2:100) {
  OU[i] <- OU[i-1]+theta*(mu-OU[i-1])+sigma*rnorm(1)
}

#plotting trajectory
plot(1:100, OU, main="Ornstein-Uhlenbeck process", type="l", lty=1, lwd=2,
     col="navy", xlab="Time", ylab="Position", first.panel=grid())

