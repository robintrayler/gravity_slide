library(tidyverse)

data <- read_csv(file = './data/model_age.csv') %>% 
  mutate(layer = fct_reorder(layer, rank, .desc = TRUE))

# text labels
summary_stats <- data %>% 
  group_by(layer) %>% 
  summarise(median = quantile(age, 0.5) %>% round(3), 
            low = quantile(age, 0.025),
            high = quantile(age, 0.975),
            rank = unique(rank)) %>% 
  mutate(minus = median - low %>% round(2),
         plus = high - median %>% round(2)) 



