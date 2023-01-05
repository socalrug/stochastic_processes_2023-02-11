#finding theoretical probability of ruin
library(rootSolve)

equation<- function(x)
   0.4-0.7*x+0.2*x^5+0.1*x^10
  
print(uniroot.all(equation, c(0,0.99)))

#estimating probability of ruin empirically
N<- matrix(NA,nrow=10,ncol=1000)
N[1,]<- 1
n.zero<- 0

set.seed(349813)
for (j in 1:1000) {
  for (i in 2:10) {
N[i,j]<- sum(sample(c(0,1,5,10), N[i-1,j], replace=TRUE, 
                    prob=c(0.4, 0.3, 0.2, 0.1)))
    if (N[i,j]==0) { 
      n.zero<- n.zero+1
      break
    }
    
  }
}

print(n.zero/1000)  