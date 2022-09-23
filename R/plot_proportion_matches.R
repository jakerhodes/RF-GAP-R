library(ggplot2)
library(tidyverse)

train_proportions_dt <- fread('experiments/proximities_split/aa_proportion_matches_train.csv')
test_proportions_dt <- fread('experiments/proximities_split/aa_proportion_matches_test.csv')

proximity_types <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')
proximity_names <- c('RF-GAP', 'Original', 'OOB', 'PBK', 'RFProxIH')

combins <- permutations(2, 5, repeats.allowed = TRUE) - 1
combins <- combins[-which(rowSums(combins) != 2), ]
combins


pair_matrix <- matrix(nrow = length(proximity_types), ncol = length(proximity_types))

rownames(pair_matrix) <- proximity_types
colnames(pair_matrix) <- proximity_types

diag(pair_matrix) <- 1

for (row in 1:nrow(combins)) {
  combination <- combins[row, ]

  included_prox <- which(combination == 1)

  proportion <- mean(train_proportions_dt[rfgap == combination[1] &
                         original == combination[2] &
                         oob      == combination[3] &
                         pbk      == combination[4] &
                         rfproxih == combination[5],
                       .('mean' = mean(proportion_matches),
                         'sd'   = sd(proportion_matches)),
                       by = c(proximity_types, 'dataset')]$mean)

  pair_matrix[included_prox[1], included_prox[2]] <- proportion
  pair_matrix[included_prox[2], included_prox[1]] <- proportion

}

pair_matrix %>%
  as.data.frame() %>%
  rownames_to_column('Proximity') %>%
  pivot_longer(-c(Proximity), names_to = 'Proximity2') %>%
  ggplot(aes(x = reorder(Proximity, match(Proximity, proximity_types)),
             y = reorder(Proximity2, match(Proximity2, proximity_types)),
             fill = value)) +
  geom_raster() +
  geom_text(aes(label = round(value, 2))) +
  scale_x_discrete(labels = proximity_names) +
  scale_y_discrete(limits = rev, labels = rev(proximity_names)) +
  scale_fill_continuous(type = 'viridis') +
  xlab('') +
  ylab('')




heatmap(pair_matrix, Colv = NA, Rowv = NA, revC = TRUE, symm = TRUE,
        labRow = proximity_names, labCol = proximity_names)



pair_dt <- as.data.table(pair_matrix)
