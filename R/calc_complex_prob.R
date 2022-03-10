# sum the probability distributions
calc_complex_prob <- function(x, ages, age_sd) {
  n <- length(ages)
  prob <- matrix(ncol = n, nrow = length(x))
  for(i in 1:n){
    prob[, i] <- dnorm(x, 
                       mean = ages[i], 
                       sd = age_sd[i])
  }
  prob <- apply(X = prob, MARGIN = 1, FUN = sum)
  return(prob)
}