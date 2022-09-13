
library(ggplot2)

node_df <- read.table('experiments/min_node_size/node_size_results.csv',
                    sep = ',', header = TRUE)


'%!in%' <- Negate('%in%')

classification_df <- node_df[node_df$dataset  %!in% c('auto-mpg', 'arrhythmia'), ]

anova <- aov(pct_no_match ~ node_size, data = node_df)
summary(anova)

scatter <- ggplot(data = node_df) +
  geom_point(aes(x = node_size, y = pct_no_match))

scatter

scatter_class <- ggplot(data = classification_df) +
  geom_point(aes(x = node_size, y = pct_no_match))

scatter_class


box <- ggplot(data = node_df) +
  geom_boxplot(aes(group = node_size, y = pct_no_match, x = node_size))

box


box.single <- ggplot(data = node_df[node_df[ , 'dataset'] == 'car', ]) +
  geom_boxplot(aes(group = node_size, y = pct_no_match, x = node_size))

box.single
