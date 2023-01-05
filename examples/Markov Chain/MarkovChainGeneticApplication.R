#specifying the transition probability matrix
tm<- matrix(c(1, 0, 0, 0.5, 0.5, 0, 0, 1, 0), nrow=3, 
ncol=3, byrow=TRUE)

#creating Markov chain object
library(markovchain)
mc<- new("markovchain", transitionMatrix=tm, 
states=c("AA", "Aa", "aa"))

#computing Markov chain characteristics
steadyStates(mc)

#computing the distribution of genes in each generation
gen<- c(0.99, 0, 0.01)
gen<- gen%*%tm
for (n in 2:10) { 
  print(n)
  print(round(gen,digits=10))
  gen<- gen%*%tm
}
