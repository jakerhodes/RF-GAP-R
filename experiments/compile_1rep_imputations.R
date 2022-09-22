#------------------------------------------------------------------------------#
#
#------------------------------------------------------------------------------#

library(data.table)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidyverse)


proximity_names <- c('RF-GAP', 'Original', 'OOB', 'PBK', 'RFProxIH')
data_df_path <- 'experiments/imputation/single_iter_results/'

data <- fread(paste0(data_df_path, 'combined_results.csv'))
data <- data[dataset != 'car']
data <- data[dataset != 'tic-tac-toe']

data[, mean_norm := .(mean(norm_error)), by = c('dataset', 'proximity_type', 'pct_missing')]
data[, sd_norm := .(sprintf("(%.2f)", sd(norm_error))), by = c('dataset', 'proximity_type', 'pct_missing')]
data[, seed := NULL]
data[, norm_error := NULL]


unique_dt <- unique(data)

wide_dt <- dcast(unique_dt, ... ~ pct_missing, value.var = c('mean_norm', 'sd_norm'))

wide_df <- as.data.frame(wide_dt)

wide_df <- wide_df[, c(1, 2, 3, 8, 4, 9, 5, 10, 6, 11, 7, 12)]

wide_df$dataset <- sub("_", " ", wide_df$dataset)

wide_df$proximity_type <- sub("oob", "OOB", wide_df$proximity_type)
wide_df$proximity_type <- sub("original", "Original", wide_df$proximity_type)
wide_df$proximity_type <- sub("rfgap", "RF-GAP", wide_df$proximity_type)
wide_df$proximity_type <- sub("pbk", "PBK", wide_df$proximity_type)
wide_df$proximity_type <- sub("rfproxih", "RFProxIH", wide_df$proximity_type)

wide_df[, 'dataset'] <- str_to_title(wide_df[, 'dataset'])

colnames(wide_df) <- c('Dataset', 'Proximity', '5%', 'SD 5%', '10%',
                       'SD 10%', '25%', 'SD 25%', '50%', 'SD 50%'
                       , '75%', 'SD 75%')

wide_df <- wide_df[order(wide_df$Dataset, match(wide_df$Proximity, proximity_names)), ]

wide_df <-
  wide_df %>%
  mutate_if(is.numeric, round, 3)

hline <- c(-1, 0, nrow(wide_df), seq(3, 54, 3))
print(xtable(wide_df, align = c('|l|', '|l|', 'c|', 'c',  'c|', 'c', 'c|',
                                 'c', 'c|', 'c', 'c|', 'c', 'r|'),
             caption = '',
             digits = c(0, 0, 0, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2)),
      include.rownames = FALSE,
      hline.after = hline,
      label = 'tab:train_test_diff')





# ggplot(data = unique_dt, aes(x = dataset,
#                                y = mean_norm,
#                                fill = proximity_type)) +
#   geom_boxplot() +
#   coord_flip() +
#   facet_wrap('pct_missing', ncol = 1)

