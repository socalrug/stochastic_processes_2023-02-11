volcanoes.data<- read.csv(file="./data/volcanoesdata.csv", header=TRUE, sep=",")

#creating date-time variable
datetime<- as.POSIXct(paste(as.Date(volcanoes.data$DATE, '%m/%d/%Y'), volcanoes.data$TIME))

#computing lag
datetime.lag<- c(0,head(datetime, -1))

#computing interarrival times (in hours)
int<- (as.numeric(datetime)-as.numeric(datetime.lag))/(3600*24)
int<- int[-1] # removing first value

#plotting the process
time<- c()
time[1]<- 0
for (i in 2:(length(int)+1)) 
  time[i]<- time[i-1]+int[i-1]
N<- 0:length(int)

plot(time,N, type="n", main="Volcano Eruption Process", 
xlab="Time", ylab="State", panel.first = grid())

segments(time[-length(time)], N[-length(time)], time[-1]-0.07, 
N[-length(time)], lwd=2, col="blue")

points(time, N, pch=20, col="blue")
points(time[-1], N[-length(time)], pch=1, col="blue")

#CHECKING IF THE PROCESS IS POISSON#
#plotting histogram
hist(int, main="", xlab="Interarrival Time", col="dodgerblue")

#binning interarrival times
binned.int<- as.factor(ifelse(int<25,"1",
ifelse(int>=25 & int<50,"2",ifelse(int>=50 & int<100,"3",
ifelse(int>=100 & int<150,"4",ifelse(int>=150 & int<200,"5",
ifelse(int>=200 & int<250,"6", "7")))))))

#computing observed frequencies
obs<- table(binned.int)

#estimating mean for exponential distribution
mean.est<- mean(int)

#computing expected frequencies
exp<- c(1:7)
exp[1]<- length(int)*(1-exp(-25/mean.est))
exp[2]<- length(int)*(exp(-25/mean.est)-exp(-50/mean.est))
exp[3]<- length(int)*(exp(-50/mean.est)-exp(-100/mean.est))
exp[4]<- length(int)*(exp(-100/mean.est)-exp(-150/mean.est))
exp[5]<- length(int)*(exp(-150/mean.est)-exp(-200/mean.est))
exp[6]<- length(int)*(exp(-200/mean.est)-exp(-250/mean.est))
exp[7]<- length(int)*exp(-250/mean.est)

obs
round(exp,1)

#computing chi-squared statistic 
print(chi.sq<- sum((obs-exp)^2/exp))

#computing p-value
print(p.value<- 1-pchisq(chi.sq, df=5))
