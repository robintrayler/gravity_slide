library(tidyverse)
# model read in the model data
data <- read_csv(file = './data/model_age.csv') %>% 
  mutate(layer = fct_reorder(layer, rank, .desc = TRUE))
geochron <- read_csv(file = './data/geochronology.csv')
# read in the likelihood densities for plotting 

layer <- unique(geochron$layer)
x <- seq(22, 24.25, length = 1000)
ridges <- list()
for(i in seq_along(layer)) {
  y <- calc_complex_prob(x, 
                         ages = geochron$age[geochron$layer == layer[i]], 
                         age_sd = geochron$`2_sd`[geochron$layer == layer[i]] / 2)
  ridges[[i]] <- data.frame(age = x, 
                            probability = y, 
                            layer = layer[i])
}


  
 ridges <- ridges %>% 
   reduce(rbind) %>% 
 mutate(layer = factor(layer, levels = c('Haycock', 'pseudotachylyte', 'basal layers')))
 
 ridges$probability[ridges$probability < 1e-12] = NA
   


# ridges <- read_csv(file = './data/likelihood_density.csv') %>% 
#   mutate(layer = fct_reorder(layer, rank, .desc = TRUE))
# ridges$probability[ridges$probability < 1e-12] = NA

# make a data frame of labels
labels <- 
  tribble(~layer,           ~age,  ~y, ~rank, 
          'Haycock',         23.15, 15,  3,
          'pseudotachylyte', 22.4,   3,  2,
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
  scale_fill_brewer(palette = 'Set2') + 
  scale_color_brewer(palette = 'Set2') + 
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
                          y = probability * 1),
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
  mutate(minus = median - low %>% round(3),
         plus = high - median %>% round(3)) 

summary_stats %>% 
  write_csv(file = 'results_summary.csv')
