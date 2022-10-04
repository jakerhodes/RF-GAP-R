library(ggplot2)
library(tidyverse)
library(gtools)

train_proportions_dt <- fread('experiments/proximities_split/aa_proportion_matches_train.csv')
test_proportions_dt <- fread('experiments/proximities_split/aa_proportion_matches_test.csv')

proximity_types <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')
proximity_names <- c('RF-GAP', 'Original', 'OOB', 'PBK', 'RFProxIH')

combins <- permutations(2, 5, repeats.allowed = TRUE) - 1
combins <- combins[-which(rowSums(combins) != 2), ]
combins


proportion_matrix <- matrix(nrow = length(proximity_types), ncol = length(proximity_types))
sd_matrix   <- matrix(nrow = length(proximity_types), ncol = length(proximity_types))

rownames(proportion_matrix) <- proximity_types
colnames(proportion_matrix) <- proximity_types

diag(proportion_matrix) <- 1
diag(sd_matrix) <- 0

for (row in 1:nrow(combins)) {
  combination <- combins[row, ]

  included_prox <- which(combination == 1)

  combination_results <- train_proportions_dt[rfgap == combination[1] &
                                             original == combination[2] &
                                             oob      == combination[3] &
                                             pbk      == combination[4] &
                                             rfproxih == combination[5],
                                           .('mean' = mean(proportion_matches),
                                             'sd'   = sd(proportion_matches)),
                                           by = c(proximity_types, 'dataset')]

  proportion <- mean(combination_results$mean)


  std <- sqrt(sum((combination_results$sd^2))/nrow(combination_results))


  proportion_matrix[included_prox[1], included_prox[2]] <- proportion
  proportion_matrix[included_prox[2], included_prox[1]] <- proportion

  sd_matrix[included_prox[1], included_prox[2]] <- std
  sd_matrix[included_prox[2], included_prox[1]] <- std

}

sd_long <- sd_matrix %>%
  as.data.frame() %>%
  rownames_to_column('Proximity') %>%
  pivot_longer(-c(Proximity), names_to = 'Proximity2')

proportion_matrix %>%
  as.data.frame() %>%
  rownames_to_column('Proximity') %>%
  pivot_longer(-c(Proximity), names_to = 'Proximity2') %>%
  ggplot(aes(x = reorder(Proximity, match(Proximity, proximity_types)),
             y = reorder(Proximity2, match(Proximity2, proximity_types)),
             fill = value)) +
  geom_raster() +
  geom_text(aes(label = paste(round(value, 2), '(', round(sd_long$value, 3), ')'))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = proximity_names) +
  scale_y_discrete(limits = rev, labels = rev(proximity_names)) +
  # scale_fill_continuous(type = 'viridis') +
  xlab('') +
  ylab('') +
  ggtitle('Proportion of Train Prediction Matches') +
  # scale_fill_gradient(low = '#e0f3db', high = '#43a2ca') # For training set
  scale_fill_gradient(low = '#bcdad6', high = '#43a2ca') # For test set



ggsave('experiments/figs/proportion_matches_train.pdf', width = 7, height = 6)
