library(tidyverse)

#specifying parameters
gen.max<- 5
prob<- 0.7

#specifying seed
set.seed(2443534)

#simulating trajectory
level.segment <- function(gen, y, branch.num) {
    
branch<- data.frame(x=c(), y=c(), xend=c(), yend=c())
gen.remaining<- gen.max-gen-1
    
if (gen.remaining < 0) return(branch)

  if (branch.num > 0) {
   branch<- rbind(branch, data.frame(x=gen, y=y, xend=gen+1, yend=y),
     level.segment(gen=gen+1, y=y, branch.num=rbinom(1, 3, prob)))
    }

 if (branch.num > 1) {
   branch<- rbind(branch, data.frame(x=gen, y=y, xend=gen+1, yend=y+3^gen.remaining),
     level.segment(gen=gen+1, y=y+3^gen.remaining, branch.num=rbinom(1, 3, prob)))
 }

 if (branch.num > 2) {
   branch<- rbind(branch, data.frame(x=gen, y=y, xend=gen+1, yend=y-3^gen.remaining),
     level.segment(gen=gen+1, y=y-3^gen.remaining, branch.num=rbinom(1, 3, prob)))
    }

    branch
  }

  bp<- level.segment(0, 0, rbinom(1, 3, prob))
  
#plotting trajectory
plot(bp[,1], bp[,2], type="n", yaxt="n", xlim=c(0,5), ylim=c(range(bp)), 
xlab="Generation", ylab="Branching Process", panel.first=grid())

segments(bp[,1], bp[,2], bp[,3], bp[,4], lwd=2, col="blue")
 

