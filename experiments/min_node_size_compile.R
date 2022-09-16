library(ggplot2)
'%!in%' <- Negate('%in%')

node_df <- read.table('experiments/min_node_size/node_size_results_no_pbk.csv',
                    sep = ',', header = TRUE)


rfgap_df <- node_df[node_df[, 'proximity_type'] == 'rfgap', ]


classification_df <- node_df[node_df$dataset  %!in% c('auto-mpg', 'arrhythmia'), ]

regression_df <- node_df[node_df$dataset  %in% c('auto-mpg', 'arrhythmia'), ]

anova <- aov(pct_no_match ~ proximity_type + node_size, data = node_df)
summary(anova)



box <- ggplot(data = classification_df) +
  geom_boxplot(aes(y = pct_no_match, x = as.factor(node_size), fill = proximity_type))

box


box.regression <- ggplot(data = regression_df) +
  geom_boxplot(aes(y = pct_no_match, x = as.factor(node_size), fill = proximity_type))

box.regression

box.single <- ggplot(data = node_df[node_df[ , 'dataset'] == 'car', ]) +
  geom_boxplot(aes(group = node_size, y = pct_no_match, x = node_size))

box.single


box.single.rfgap <- ggplot(data = rfgap_df[rfgap_df[ , 'dataset'] == 'ecoli', ]) +
  geom_boxplot(aes(group = node_size, y = pct_no_match, x = node_size))

box.single.rfgap
