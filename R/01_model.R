# load required packages
library(tidyverse)
source('./R/00_required_functions.R')

# load the data ---------------------------------------------------------------
data <- read_csv(file = './data/geochronology.csv') %>% 
  mutate(age_sd = `2_sd` / 2) %>% 
  select(position,
         layer,
         rank,
         unit,
         age,
         age_sd)

# Set up the model ------------------------------------------------------------
iterations <- 100000
burn = iterations / 10

# order the geochronology by rank
o <- order(data$rank)
data   <- data[o, ]

# set up some model storage
rank   <- unique(data$rank)
thetas <- matrix(nrow = iterations, ncol = length(rank))
LL     <- matrix(nrow = iterations, ncol = length(rank))

# pick starting values. they don't need to be very accurate 
for(i in seq_along(rank)) {
  thetas[1, i] <- truncated_random_normal(mean = mean(data$age[data$rank == rank[i]]),
                                          sd = mean(data$age_sd[data$rank == rank[i]]),
                                          low = 0,
                                          high = 1e10)
}

# keep things in stratigraphic order to start
thetas[1, ] <- sort(thetas[1, ], decreasing = TRUE) 

# run the model ---------------------------------------------------------------
pb <- progress::progress_bar$new(total = iterations,
                                 format = '[:bar] :percent eta: :eta')
for(j in 2:iterations) {
  pb$tick() # update progress bar
  
  # save the previous iteration
  current_theta <- thetas[j - 1, ]
  
  # update the thetas
  for(i in seq_along(rank)) {
    # use the adaptive MCMC sampler
    theta_proposed <- adaptive_update(chain = thetas[, i],
                                      i = j,
                                      start_index = burn / 2,
                                      younger_truncation = 
                                        ifelse(test = rank[i] == rank[length(rank)], 
                                               yes = 0, 
                                               no = current_theta[i + 1]),
                                      older_truncation = 
                                        ifelse(test = rank[i] == rank[1],
                                               yes = 1000,
                                               no = current_theta[i - 1]))
    
    # calculate log likelihood ----------------------------
    # proposal
    proposed_LL <- calc_complex_prob(x    = theta_proposed,
                                   ages   = data$age[data$rank == rank[i]],
                                   age_sd = data$age_sd[data$rank == rank[i]]) %>% 
      log()
    # current value 
    current_LL <- calc_complex_prob(x     = thetas[j - 1, i],
                                   ages   = data$age[data$rank == rank[i]],
                                   age_sd = data$age_sd[data$rank == rank[i]]) %>% 
      log()
    
    # calculate probability ratio
    p <- proposed_LL - current_LL
    
    # accept or reject using an metropolis-hastings algorithm
    if(!is.na(p)) {
      if(!is.infinite(p)) {
        if(exp(p) > runif(1)) {
          current_theta[i] <- theta_proposed
        }
      }
    }
  }
  # add the results to the chain
  thetas[j, ] <- current_theta
}

# aggregate the data into a data frame
thetas <- thetas %>% 
  as.data.frame() %>% 
  set_names(nm = unique(data$layer)) %>% 
  add_column(iteration = 1:iterations) %>% 
  pivot_longer(cols = unique(data$layer),
               names_to = 'layer', 
               values_to = 'age') %>% 
  right_join(data %>% select(layer, rank), 
             by = 'layer')
  
# save the results
write_csv(x = thetas,
          file = './data/model_age.csv')