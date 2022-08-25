library(tidyverse)
library(cowplot)
source('./R/00_required_functions.R')

# model read in the model data
table_1 <- read_csv(file = './results/table_1_model_age.csv') %>% 
  mutate(layer = fct_reorder(layer, 
                             rank, 
                             .desc = TRUE))

table_2 <- read_csv(file = './results/table_2_model_age.csv') %>% 
  mutate(layer = fct_reorder(layer, 
                             rank, 
                             .desc = TRUE))

geochron_1 <- read_csv(file = './data/geochronology_table_1.csv')
geochron_2 <- read_csv(file = './data/geochronology_table_2.csv')


# make likelihood PDS's for plotting

layer_1 <- unique(geochron_1$layer)
x <- seq(22, 24.25, length = 1000)
ridges_1 <- list()
for(i in seq_along(layer_1)) {
  y <- calc_complex_prob(x, 
                         ages = geochron_1$age[geochron_1$layer == layer_1[i]], 
                         age_sd = geochron_1$`2_sd`[geochron_1$layer == layer_1[i]] / 2)
  ridges_1[[i]] <- data.frame(age = x, 
                            probability = y, 
                            layer = layer_1[i])
}

ridges_1 <- ridges_1 %>% 
  reduce(rbind) %>% 
  mutate(layer = factor(layer, 
                        levels = c('Haycock', 
                                   'pseudotachylyte', 
                                   'basal layers')))
# drop really low probabilties
ridges_1$probability[ridges_1$probability < 1e-12] = NA


layer_2 <- unique(geochron_2$layer)

x <- seq(22, 24.25, length = 1000)
ridges_2 <- list()
for(i in seq_along(layer_2)) {
  y <- calc_complex_prob(x, 
                         ages = geochron_2$age[geochron_2$layer == layer_2[i]], 
                         age_sd = geochron_2$`2_sd`[geochron_2$layer == layer_2[i]] / 2)
  ridges_2[[i]] <- data.frame(age = x, 
                            probability = y, 
                            layer = layer_2[i])
}

ridges_2 <- ridges_2 %>% 
  reduce(rbind) %>% 
  mutate(layer = factor(layer, 
                        levels = c('Haycock', 
                                   'pseudotachylyte', 
                                   'basal layers')))
# drop really low probabilties
ridges_2$probability[ridges_2$probability < 1e-12] = NA


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
figure_a <- table_1 %>% 
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
  geom_line(data = ridges_1,
            mapping = aes(x = age,
                          y = probability * 1),
            inherit.aes = FALSE,
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



figure_b <- table_2 %>% 
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
  geom_line(data = ridges_2,
            mapping = aes(x = age,
                          y = probability * 1),
            inherit.aes = FALSE,
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


pdf(file = './results/figure.pdf',
    width = 6.5,
    height = 6.5)
plot_grid(figure_a,
          figure_b, 
          ncol = 1)
dev.off()

# Calculate summary statistics ------------------------------------------------
summary_stats_table_1 <- table_1 %>% 
  group_by(layer) %>% 
  summarise(median = quantile(age, 0.5) %>% round(3), 
            low = quantile(age, 0.025),
            high = quantile(age, 0.975),
            rank = unique(rank)) %>% 
  mutate(minus = median - low,
         plus = high - median) %>% 
  mutate_if(is.numeric, round,2 ) %>% 
  select(layer, 
         median, 
         plus,
         minus)
  

summary_stats_table_2 <- table_2 %>% 
  group_by(layer) %>% 
  summarise(median = quantile(age, 0.5) %>% round(3), 
            low = quantile(age, 0.025),
            high = quantile(age, 0.975),
            rank = unique(rank)) %>% 
  mutate(minus = median - low,
         plus = high - median) %>% 
  mutate_if(is.numeric, round,2 ) %>% 
  select(layer, 
         median, 
         plus,
         minus)

summary_stats_table_1 %>% 
  write_csv(file = './results/summary_stats_table_1.csv')
summary_stats_table_2 %>% 
  write_csv(file = './results/summary_stats_table_2.csv')

