# This function is a simple implementation of the adaptive MCMC algorithm of
# Haario et al. (2001)

adaptive_update <- function(chain,
                            i,
                            start_index = 1000,
                            initial_Cd = 0.01,
                            younger_truncation = -1e10,
                            older_truncation = 1e10) {

  # set some global parameters ------------------------------------------------
  esp <- 1e-5 # keep things from going to zero
  S_d = 2.4^2 # scaling factor
  C_d <- var(chain[1:(i-1)]) * S_d + S_d * esp %*% diag(1)

  if(i >= start_index) {
    # calculate the proposal variance -----------------------------------------
    # per Haario et al., (2001)

    x_proposed <- truncated_random_normal(
      mean = chain[i - 1],
      sd = sqrt(C_d),
      low = younger_truncation,
      high = older_truncation)

  } else if (i < start_index) {
    x_proposed <- truncated_random_normal(
      mean = chain[i - 1],
      sd = sqrt(initial_Cd),
      low = younger_truncation,
      high = older_truncation)
  }
  return(x_proposed)
}

##-----------------------------------------------------------------------------
## Function for truncated random normal with a mean and sd
truncated_random_normal <- function(mean,
                                    sd,
                                    low  = 1e-10,
                                    high = 1e10){
  lowlimold <- (low - mean) / sd
  upplimold <- (high - mean) / sd
  y <- truncated_standard_normal(lowlimold, upplimold)
  newvalue <- mean + sd * y
  return(newvalue)
}

# truncated normal functions --------------------------------------------------
truncated_standard_normal <- function(a, b){
  # this function draws a random number from a normal distribution centered at
  # 0 with a mean of 1, with truncations at a and b.
  # INPUTS: a = lower truncation
  #         b = upper truncation
  # OUTPUT: x = random number
  accept = FALSE
  A <- atan(a)
  B <- atan(b)
  maxA <- exp((-(a^2) / 4)) / cos(A)
  maxB <- exp((-(b^2) / 4)) / cos(B)
  maxR <- max(maxA, maxB)
  if((a < 1)& (b > 1)){
    maxR <- exp(-.25) * sqrt(2)
  }
  
  while (accept == FALSE) {
    r2 <- runif(1, 0, 1)
    r <- sqrt(r2) * maxR
    th <- runif(1, A, B)
    u <- r * cos(th)
    x <- tan(th)
    accept <- (x^2) < (log(u) * -4)
  }
  return(x)
}

# sum probability distributions ---------------------------------------------------------
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