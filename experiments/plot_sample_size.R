library(data.table)
library(ggplot2)
library(RColorBrewer)

errors_dt <- fread('experiments/sample_size/errors.csv')
aggregate_dt <- errors_dt[, .('Mean_Train' = mean(Train_Error),
                              'SD_Train'   = sd(Train_Error),
                              'Mean_Test'  = mean(Test_Error),
                              'SD_Test'    = sd(Test_Error),
                              'Mean_RF_OOB' = mean(RF_OOB),
                              'Mean_RF_Test' = mean(RF_Test),
                              'SD_RF_OOB' = sd(RF_OOB),
                              'SD_RF_Test' = sd(RF_Test)
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


ggplot(data = aggregate_dt, aes(x = Sample_Size,
                                y = Mean_Train,
                                color = Proximity)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean_Train - SD_Train,
                    ymax = Mean_Train + SD_Train),
                width = .05) +
  ylim(c(-.01, .40)) +
  scale_x_continuous(trans = 'log10', breaks = sizes) +
  coord_trans(x = 'log10') +
  xlab('Sample Size') +
  ylab('Proportion') +
  scale_shape_manual(values = shapes, breaks = proximity_order) +
  scale_color_manual(values = colors, breaks = proximity_order) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = c(.9, .85),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  ggtitle('Unmatched Training Labels')


#------------------------------------------------------------------------------#
#                              Test Errors
#------------------------------------------------------------------------------#

ggplot(data = aggregate_dt, aes(x = Sample_Size,
                                y = Mean_Test,
                                color = Proximity)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean_Test - SD_Test,
                    ymax = Mean_Test + SD_Test),
                width = .05) +
  ylim(c(0, .15)) +
  scale_x_continuous(trans = 'log10', breaks = sizes) +
  coord_trans(x = 'log10') +
  xlab('Sample Size') +
  ylab('Proportion') +
  scale_shape_manual(values = shapes, breaks = proximity_order) +
  scale_color_manual(values = colors, breaks = proximity_order) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = c(.9, .85),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  ggtitle('Unmatched Test Labels')

#------------------------------------------------------------------------------#
#                       RF Error Rates
#------------------------------------------------------------------------------#
ggplot(data = aggregate_dt, aes(x = Sample_Size,
                             y = Mean_RF_Test)) +
  geom_point(color = 'red') +
  geom_errorbar(aes(ymin = Mean_RF_Test - SD_RF_Test,
                    ymax = Mean_RF_Test + SD_RF_Test,
                    color = 'Test'),
                width = .1) +

  geom_point(aes(y = Mean_RF_OOB)) +

  geom_errorbar(aes(ymin = Mean_RF_OOB - SD_RF_OOB,
                    ymax = Mean_RF_OOB + SD_RF_OOB,
                    color = 'Train'),
                width = .05) +
  scale_x_continuous(trans = 'log10') +
  coord_trans(x = 'log10') +
  xlab('Sample Size') +
  ylab('Error') +
  ylim(c(.15, .4)) +
  ggtitle('Random Forest Errors by Sample Size') +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title  = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = c(.9, .85),
        plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_color_manual(values = c('red', 'black'), name = 'Error Type')

