##n = sample size, nsamp = number of sample repeats, dist = distribution type, accepts either "n" for normale or "e" for exponential
## note that you MUST include the " marks around n and e in the function call for it to take properly
sampletake <- function(n, nsamp, dist) {
  ##start IF statement, these if statements generate the sample pool according to the distribution, either normal or exponential
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
  ## Write the standard deviations and means. 
  sampleSD <- sd(samplepool)
  samplemean <- mean(samplepool)
  
  ##we must initalize the vector with the correct size before using it.
  samplecontainer <- numeric(nsamp)
  
  ##start FOR loop, this loop draws samples from our samplepool for the bootstrap
  for (i in 1:nsamp){
    thissample <- sample(samplepool, size = length(samplepool), replace = TRUE)
    samplecontainer[i] <- mean(thissample)
  }
  ##end FOR loop
  
  ##start IF statement now we render the histograms of the bootstrapping results. the if-else-if statement is mostly for differing graph labels
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
##end function

##Conclusions:
## As we can see no matter if the distribution was normal or exponential the bootstrap sample we took remained approximately normal
