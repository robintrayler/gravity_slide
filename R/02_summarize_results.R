library(tidyverse)
# model read in the model data
data <- read_csv(file = './data/model_age.csv') %>% 
  mutate(layer = fct_reorder(layer, rank, .desc = TRUE))

# read in the likelihood densities for plotting 

ridges <- read_csv(file = './data/likelihood_density.csv') %>% 
  mutate(layer = fct_reorder(layer, rank, .desc = TRUE))
ridges$probability[ridges$probability < 1e-12] = NA

# make a data frame of labels
labels <- 
  tribble(~layer,           ~age,  ~y, ~rank, 
          'Haycock',         23.15, 15,  4,
          'pseudotachylyte', 22.4,   3,  3,
          'andesite',        23.52,  2.5,  2, 
          'basal layers',    24.2,   1,  1) %>% 
  mutate(layer = fct_reorder(layer, 
                             rank, 
                             .desc = TRUE))

# plot the model posterior
figure <- data %>% 
  ggplot(mapping = aes(x = age, 
                       fill = layer)) + 
  facet_grid(layer ~ ., 
             scales = 'free_y') + 
  theme_bw() + 
  scale_fill_brewer(palette = 'Spectral') + 
  scale_color_brewer(palette = 'Spectral') + 
  theme(legend.position = 'none',
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y  = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(), 
        strip.text = element_blank()) + 
  geom_line(data = ridges,
            mapping = aes(x = age, 
                          y = probability * 2.5),
            inherit.aes = FALSE,
            # size = 1,
            alpha = 0.75) + 
  geom_density(color = NA,
               alpha = 0.75) +
  xlab('Age (Ma)') + 
  ylab('Density') + 
  geom_text(data = labels,
            mapping = aes(x = age, 
                          y = y,
                          label = layer,
                          color = layer),
            alpha = 1)

# Calculate summary statistics ------------------------------------------------
summary_stats <- data %>% 
  group_by(layer) %>% 
  summarise(median = quantile(age, 0.5) %>% round(3), 
            low = quantile(age, 0.025),
            high = quantile(age, 0.975),
            rank = unique(rank)) %>% 
  mutate(minus = median - low %>% round(2),
         plus = high - median %>% round(2)) 


