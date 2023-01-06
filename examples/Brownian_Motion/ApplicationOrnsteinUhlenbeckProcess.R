gasprice.data <- read.csv(file=file.path("examples", "Brownian_Motion", "data", "gaspricedata.csv"),
                          header=TRUE, sep=",")

#estimating parameters
inc <- gasprice.data$Price[-1]-head(gasprice.data$Price,-1)
fit <- glm(inc ~ head(gasprice.data$Price,-1))
theta.hat <- -fit$coefficients[2]
mu.hat <- fit$coefficients[1]/theta.hat
sigma.hat <- sigma(fit)

#specifying seed
set.seed(9467108)

#simulating OU process
OU <- c()
OU[1] <- gasprice.data$Price[1]

for (i in 2:length(gasprice.data$Date)) {
  OU[i] <- OU[i-1]+theta.hat*(mu.hat-OU[i-1])+sigma.hat*rnorm(1)
}

#plotting trajectories
plot(as.Date(gasprice.data$Date), gasprice.data$Price, type="l", lty=1, lwd=2, col=3,
             ylim=c(0,8), xlab="Time", ylab="Natural gas price", first.panel=grid())
lines(as.Date(gasprice.data$Date), OU, lwd=2, col=4)
legend("topright", c("Actual price", "Simulated price"), lty=1, col=3:4)

