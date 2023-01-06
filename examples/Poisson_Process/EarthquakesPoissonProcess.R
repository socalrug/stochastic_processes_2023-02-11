eq.data<- read.csv(file="./data/earthquakedata2012-2018.csv", 
header=TRUE, sep=",")

#creating date-time variable
datetime<- as.POSIXct(paste(as.Date(eq.data$DATE), eq.data$TIME))

#computing lag
datetime.lag<- c(0,head(datetime, -1))

#computing interarrival times (in hours)
int.time<- (as.numeric(datetime)-as.numeric(datetime.lag))/3600

#removing first value
int.time<- int.time[-1] 

#removing immediate aftershocks
int<- int.time[int.time>3]

#plotting the process
time<- c()
time[1]<- 0
for (i in 2:(length(int)+1)) 
  time[i]<- time[i-1]+int[i-1]
N<- 0:length(int)

plot(time,N, type="n", main="Earthquake Process", 
xlab="Time", ylab="State", panel.first = grid())

segments(time[-length(time)], N[-length(time)], time[-1]-0.07, 
N[-length(time)], lwd=2, col="blue")

points(time, N, pch=20, col="blue")
points(time[-1], N[-length(time)], pch=1, col="blue")

#CHECKING IF THE PROCESS IS POISSON#
#plotting histogram
hist(int, main="", col="dark magenta", xlab="Interarrival Time")

#binning interarrival times
binned.int<- as.factor(ifelse(int<40,"1",
ifelse(int>=40 & int<80,"2",ifelse(int>=80 & int<120,"3",
ifelse(int>=120 & int<160,"4",ifelse(int>=160 & int<200,"5",
ifelse(int>=200 & int<240,"6","7")))))))

#computing observed frequencies
obs<- table(binned.int)

#estimating mean for exponential distribution
mean.est<- mean(int)

#computing expected frequencies
exp<- c(1:7)
exp[1]<- length(int)*(1-exp(-40/mean.est))
exp[2]<- length(int)*(exp(-40/mean.est)-exp(-80/mean.est))
exp[3]<- length(int)*(exp(-80/mean.est)-exp(-120/mean.est))
exp[4]<- length(int)*(exp(-120/mean.est)-exp(-160/mean.est))
exp[5]<- length(int)*(exp(-160/mean.est)-exp(-200/mean.est))
exp[6]<- length(int)*(exp(-200/mean.est)-exp(-240/mean.est))
exp[7]<- length(int)*exp(-240/mean.est)

obs
round(exp,1)

#computing chi-squared statistic 
print(chi.sq<- sum((obs-exp)^2/exp))

#computing p-value
print(p.value<- 1-pchisq(chi.sq, df=5))
