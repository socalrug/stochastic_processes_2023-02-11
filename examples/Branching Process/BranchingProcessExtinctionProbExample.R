N<- matrix(NA,nrow=100,ncol=1000)
N[1,]<- 1
p<- c(0.2, 0.5, 0.3)
n.zero<- 0

set.seed(339595)
for (j in 1:1000) {
 for (i in 2:100) {
  N[i,j]<- sum(sample(0:2, N[i-1,j], replace=TRUE, prob=p))
   if (N[i,j]==0) { 
     n.zero<- n.zero+1
   break
   }

}
}
  
print(n.zero/1000)





  
