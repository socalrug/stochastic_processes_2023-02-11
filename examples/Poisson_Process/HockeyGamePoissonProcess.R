#computing probability of tied scores
sum <- 0
for(n in 0:15) {
  sum <- sum+30^n/(factorial(n))^2
}

sum*exp(-11)

#computing probability that team A wins
sum.n <- 0
for (n in 0:15) {
  sum.k <- 0
  for (k in 1:15) {
    sum.k <- sum.k+6^k/factorial(n+k)
  }
  sum.n <- sum.n + 30^n/factorial(n)*sum.k
}

print(sum.n*exp(-11))

