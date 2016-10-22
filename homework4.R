sampletake <- function(n, nsamp, dist) {
  
  ##n = sample size, nsamp = number of sample repeats, dist = distribution type, accepts either "n" for normale or "e" for exponential
  
  ##start IF statement
  if (dist == "n") 
  {
    samplepool <- rnorm(n, 50, 4) 
    hist(samplepool, n, col = "red")
  }
  else if (dist == "e")
  {
    samplepool <- rexp(n)
    hist(samplepool, n, col = "red")
  }
  ##end IF statement
  sampleSD <- sd(samplepool)
  samplemean <- mean(samplepool)
  ##we must initalize the vector with the correct size before using it.
  samplecontainer <- numeric(nsamp)
  
  ##start FOR loop
  for (i in 1:nsamp){
    thissample <- sample(samplepool, size = length(samplepool), replace = TRUE)
    samplecontainer[i] <- mean(thissample)
  }
  ##end FOR loop
  
  ##start IF statement
  if (dist == "n") 
  {
    hist(samplecontainer, main = paste("Normal distribution bootstrap"), n, col = "green")
  }
  else if (dist == "e")
  {
    hist(samplecontainer, main = paste("Exponential distribution bootstrap"), n, col = "green")
  }
  
  return(c(sampleSD, samplemean, samplecontainer))
}

