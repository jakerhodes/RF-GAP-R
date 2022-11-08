library(ggplot2)
library(gridExtra)
library(ggpubr)

'%!in%' <- Negate('%in%')

node_df <- read.table('experiments/min_node_size/node_size_results.csv',
                    sep = ',', header = TRUE)

proximity_order <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')
proximity_names <- c('RF-GAP', 'Original', 'OOB', 'PBK', 'RFProxIH')


rfgap_df <- node_df[node_df[, 'proximity_type'] == 'rfgap', ]


classification_df <- node_df[node_df$dataset  %!in% c('auto-mpg', 'arrhythmia'), ]
regression_df <- node_df[node_df$dataset  %in% c('auto-mpg', 'arrhythmia'), ]


anova <- aov(pct_no_match ~ proximity_type + node_size, data = node_df)
summary(anova)


#------------------------------------------------------------------------------#
#                       Generating Boxplots
#------------------------------------------------------------------------------#
palette <- brewer.pal(6, 'Dark2')
display.brewer.pal(6, 'Dark2')

colors <- palette[c(1, 5, 2, 3, 6)]
colors_4 <- palette[c(1, 5, 2, 6)]

box <- ggplot(data = classification_df) +
  geom_boxplot(aes(y = pct_no_match, x = as.factor(node_size), fill = reorder(proximity_type, match(proximity_type, proximity_order)))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(labels = proximity_names, values = colors) +

  theme(legend.position = 'right',
        legend.text     = element_text(size = 15),
        legend.title    = element_text(size = 17),
        title           = element_text(size = 18),
        # axis.title      = element_blank(),
        axis.text       = element_text(size = 12),
        # axis.ticks      = element_text(size = 12),
        axis.title      = element_text(size = 14),
        plot.margin     = margin(0.01, 0.01, 0.01, 0.01, 'cm'),
        panel.border    = element_rect(color = 'black', fill = NA),
        plot.title      = element_text(hjust = 0.5)) +


  xlab('Minimum Node Size') +
  ylab('|RF Error - Proximity Error|') +
  labs(fill = '') +
  ggtitle('Classification')

box


box.regression <- ggplot(data = regression_df) +
  geom_boxplot(aes(y = pct_no_match, x = as.factor(node_size), fill = reorder(proximity_type, match(proximity_type, proximity_order)))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(labels = proximity_names, values = colors_4) +

  theme(legend.position = 'right',
        legend.text     = element_text(size = 15),
        legend.title    = element_text(size = 17),
        title           = element_text(size = 18),
        # axis.title      = element_blank(),
        axis.text       = element_text(size = 12),
        # axis.ticks      = element_text(size = 12),
        axis.title      = element_text(size = 14),
        plot.margin     = margin(0.01, 0.01, 0.01, 0.01, 'cm'),
        panel.border    = element_rect(color = 'black', fill = NA),
        plot.title      = element_text(hjust = 0.5)) +

  xlab('Minimum Node Size') +
  ylab('|RF Error - Proximity Error|') +
  labs(fill = '') +
  ggtitle('Regression')

box.regression


ggarrange(box, box.regression,
          common.legend = TRUE,
          nrow = 2) +

  theme(plot.margin = margin(0.1, 0.01, 0.1, 0.01, 'cm'))

# TODO: Compile these together with single legend


ggsave('experiments/min_node_size/figs/node_size_boxplots.pdf', width = 6, height = 10, dpi = 1200)


error_plot <- ggplot(data = classification_df) +
  geom_boxplot(aes(y = prox_error, x = as.factor(node_size), fill = reorder(proximity_type, match(proximity_type, proximity_order)))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(labels = proximity_names, values = colors) +

  theme(legend.position = 'right',
        legend.text     = element_text(size = 15),
        legend.title    = element_text(size = 17),
        title           = element_text(size = 18),
        # axis.title      = element_blank(),
        axis.text       = element_text(size = 12),
        # axis.ticks      = element_text(size = 12),
        axis.title      = element_text(size = 14),
        plot.margin     = margin(0.01, 0.01, 0.01, 0.01, 'cm'),
        panel.border    = element_rect(color = 'black', fill = NA),
        plot.title      = element_text(hjust = 0.5)) +


  xlab('Minimum Node Size') +
  ylab('Proximity-Weighted Prediction Error') +
  labs(fill = '') +
  ggtitle('Classification')

error_plot

error.regression <- ggplot(data = regression_df) +
  geom_boxplot(aes(y = prox_error, x = as.factor(node_size), fill = reorder(proximity_type, match(proximity_type, proximity_order)))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(labels = proximity_names, values = colors_4) +

  theme(legend.position = 'right',
        legend.text     = element_text(size = 15),
        legend.title    = element_text(size = 17),
        title           = element_text(size = 18),
        # axis.title      = element_blank(),
        axis.text       = element_text(size = 12),
        # axis.ticks      = element_text(size = 12),
        axis.title      = element_text(size = 14),
        plot.margin     = margin(0.01, 0.01, 0.01, 0.01, 'cm'),
        panel.border    = element_rect(color = 'black', fill = NA),
        plot.title      = element_text(hjust = 0.5)) +

  xlab('Minimum Node Size') +
  ylab('MSE of Proximity Predictions') +
  labs(fill = '') +
  ggtitle('Regression')

error.regression

ggarrange(error_plot, error.regression,
          common.legend = TRUE,
          nrow = 1) +

  theme(plot.margin = margin(0.1, 0.01, 0.1, 0.01, 'cm'))

# TODO: Compile these together with single legend


ggsave('experiments/min_node_size/figs/node_size_errors.pdf', width = 10, height = 6, dpi = 1200)
