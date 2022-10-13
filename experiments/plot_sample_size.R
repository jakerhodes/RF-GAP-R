library(data.table)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)

errors_dt <- fread('experiments/sample_size/errors.csv')
aggregate_dt <- errors_dt[, .('Mean_Train'   = mean(Train_Error),
                              'SD_Train'     = sd(Train_Error),
                              'Mean_Test'    = mean(Test_Error),
                              'SD_Test'      = sd(Test_Error),
                              'Mean_RF_OOB'  = mean(RF_OOB),
                              'Mean_RF_Test' = mean(RF_Test),
                              'SD_RF_OOB'    = sd(RF_OOB),
                              'SD_RF_Test'   = sd(RF_Test)
                              ),
                          by = c('Proximity', 'Sample_Size')]

errors_df <- as.data.table(errors_dt)

palette <- brewer.pal(6, 'Dark2')
display.brewer.pal(6, 'Dark2')

proximity_order <- c('RF-GAP', 'Original', 'OOB')
colors <- palette[c(1, 5, 2)]
shapes <- c(17, 22, 24)

sizes <- c(100, 200, 500, 1000, 2000, 5000, 10000)

#------------------------------------------------------------------------------#
#                              Train Errors
#------------------------------------------------------------------------------#


train_g <- ggplot(data = aggregate_dt, aes(x = Sample_Size,
                                y = Mean_Train,
                                color = Proximity)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean_Train - SD_Train / sqrt(20),
                    ymax = Mean_Train + SD_Train / sqrt(20)),
                width = .05) +
  ylim(c(-.01, .25)) +
  scale_x_continuous(trans = 'log10', breaks = sizes) +
  coord_trans(x = 'log10') +
  xlab('Sample Size') +
  ylab('Proportion of Unmatched Labels') +
  scale_shape_manual(values = shapes, breaks = proximity_order) +
  scale_color_manual(values = colors, breaks = proximity_order) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = c(.85, .85),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  ggtitle('Unmatched OOB Labels')

train_g

ggsave('experiments/sample_size/sample_size_train.pdf', train_g, width = 6, height = 6)


#------------------------------------------------------------------------------#
#                              Test Errors
#------------------------------------------------------------------------------#

test_g <- ggplot(data = aggregate_dt, aes(x = Sample_Size,
                                y = Mean_Test,
                                color = Proximity)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean_Test - SD_Test / sqrt(20),
                    ymax = Mean_Test + SD_Test / sqrt(20)),
                width = .05) +
  ylim(c(-.01, .25)) +
  scale_x_continuous(trans = 'log10', breaks = sizes) +
  coord_trans(x = 'log10') +
  xlab('Sample Size') +
  ylab('Proportion of Unmatched Labels') +
  scale_shape_manual(values = shapes, breaks = proximity_order) +
  scale_color_manual(values = colors, breaks = proximity_order) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = c(.85, .85),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  ggtitle('Unmatched Test Labels')

test_g

ggsave('experiments/sample_size/sample_size_test.pdf', test_g, width = 6, height = 6)

#------------------------------------------------------------------------------#
#                       Combined Train/Test
#------------------------------------------------------------------------------#

ggarrange(train_g, test_g + ylab(''),
          common.legend = TRUE,
          nrow = 1,
          align = 'v') +

  theme(plot.margin = margin(0.01, 0.01, 0.01, 0.01, 'cm'))


ggsave('experiments/sample_size/sample_size_combined.pdf', width = 12, height = 6)
#------------------------------------------------------------------------------#
#                       RF Error Rates
#------------------------------------------------------------------------------#

rf_g <- ggplot(data = aggregate_dt, aes(x = Sample_Size,
                             y = Mean_RF_Test)) +
  geom_point(color = 'red') +
  geom_errorbar(aes(ymin = Mean_RF_Test - SD_RF_Test/ sqrt(20),
                    ymax = Mean_RF_Test + SD_RF_Test / sqrt(20),
                    color = 'Test'),
                width = .05) +

  geom_point(aes(y = Mean_RF_OOB)) +

  geom_errorbar(aes(ymin = Mean_RF_OOB - SD_RF_OOB / sqrt(20),
                    ymax = Mean_RF_OOB + SD_RF_OOB / sqrt(20),
                    color = 'Train'),
                width = .05) +
  scale_x_continuous(trans = 'log10', breaks = sizes) +
  coord_trans(x = 'log10') +
  xlab('Sample Size') +
  ylab('Error Rate') +
  ylim(c(.15, .275)) +
  ggtitle('Random Forest Errors by Sample Size') +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = c(.85, .85),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_color_manual(values = c('red', 'black'), name = 'Error Type')

rf_g

ggsave('experiments/sample_size/sample_size_rf.pdf', rf_g, width = 6, height = 6)
