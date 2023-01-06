stock.data<- read.csv(file="./data/AMZN.csv", header=TRUE, sep=",")

date<- as.POSIXct(stock.data$Date)
price<- stock.data$Close

price1<- price[-1]
price1.lag<- head(price, -1)
price.diff<- price1-price1.lag

#estimating parameters
print(mu.hat<- mean(price.diff))
print(sigma.hat<- sd(price.diff))

#specifying Brownian motion as vector
BM<- c()

#specifying initial value
BM[1]<- price[1]

#specifying seed
set.seed(4307347)

#simulating Brownian motion with drift and volatility
for (i in 2:length(price)) 
  BM[i]<- mu.hat+BM[i-1] + sigma.hat*rnorm(1)
  
#plotting actual and simulated trajectories
plot(date, price, type="l", lty=1, lwd=2, col="blue", ylim=range(cbind(price, BM)),
xlab="Time", ylab="Stock price", first.panel=grid())
lines(date, BM, lwd=2, col="green")
legend("topright", c("Actual price", "Simulated price"), lty=1, 
col=c("blue", "green"))
