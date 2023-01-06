####### PLOTTING DIAGRAM #######

#specifying transition probability matrix
tm <- matrix(c(0.4, 0.2, 0.4, 0.1, 0.4, 0.5, 0.5, 0.3, 0.2),
             nrow=3, ncol=3, byrow=TRUE)

#transposing transition probability matrix
tm.tr <- t(tm)

#plotting diagram
library(diagram)
plotmat(tm.tr, pos=c(1,2), arr.length=0.3, arr.width=0.1, box.col="magenta",
        box.lwd=1, box.prop=0.5, box.size=0.12, box.type="circle", cex.txt=0.8,
        lwd=1, self.cex=0.6, self.shiftx=0.17, self.shifty=-0.01)

### SIMULATING TRAJECTORIES USING BUILT-IN FUNCTION RMARKOVCHAIN()

#creating Markov chain object
library(markovchain)
mc <- new("markovchain", transitionMatrix=tm, states=c("1", "2", "3"))

#specifying total number of steps
nsteps <- 25

#specifying initial probability
p0 <- c(1/3, 1/3, 1/3)

#specifying matrix containing states
MC.states <- matrix(NA, nrow=nsteps, ncol=2)

#specifying seed
set.seed(900455)

#simulating trajectories
for (i in 1:2){
  state0 <- sample(1:3, 1, prob=p0)
  MC.states[,i] <- rmarkovchain(n=nsteps-1, object=mc, t0=state0,
  include.t0=TRUE)
}

#plotting simulated trajectories
matplot(MC.states, type="l", lty=1, lwd=2, col=c("magenta","purple"),
        main="Simulated Trajectories", xaxt="n", yaxt="n", ylim=c(1,3),
        ylab="State", xlab="Step", panel.first=grid())

axis(side=1, at=c(1,5,10,15,20,25))
axis(side=2, at=c(1,2,3))

points(1:nsteps, MC.states[,1], pch=16, col="magenta")
points(1:nsteps, MC.states[,2], pch=16, col="purple")


### COMPUTING STEADY-STATE PROBABILITIES
round(steadyStates(mc), digits=4)


### APPROXIMATING STEADY-STATE PROBABILITIES
#specifying total number of steps
nsteps <- 15

#specifying matrix containing probabilities
probs <- matrix(NA, nrow=nsteps, ncol=3)

#computing probabilities pi_n
probs[1,]  <- p0
for(n in 2:nsteps) {
  probs[n,] <- probs[n-1,]%*%tm
}

#plotting probabilities vs. step by state
matplot(probs, type="l", lty=1, lwd=2, col=c("salmon","dodgerblue","light green"),
        main="Convergence to Steady-state Probabilities", ylim=c(0.2,0.6),
        xlab="Step",ylab="Probability", panel.first = grid())
legend("topright", c("State 1", "State 2", "State 3"), lty=1, lwd=2,
       col=c("salmon","dodgerblue","light green"))

print(probs)
