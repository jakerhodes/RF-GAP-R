library(ggplot2)
library(data.table)
library(RColorBrewer)
library(ggpubr)

library(stringr)
library(dplyr)
library(tidyverse)

fig_path <- 'experiments/figs/'
proximity_path <- 'experiments/proximities_split/'

errors <- fread(paste0(proximity_path, 'aggregated_errors.csv'))
differences <- fread(paste0(proximity_path, 'aggregated_differences.csv'))

diff_names <- unique(differences$Proximity)


differences[Proximity == 'rfgap_diff', Proximity := 'RF-GAP']
differences[Proximity == 'original_diff', Proximity := 'Original']
differences[Proximity == 'oob_diff', Proximity := 'OOB']
differences[Proximity == 'pbk_diff', Proximity := 'PBK']
differences[Proximity == 'rfproxih_diff', Proximity := 'RFProxIH']


# TODO: Add this order to the imputations
proximity_names <- c('RF', 'RF-GAP', 'Original', 'OOB', 'PBK', 'RFProxIH')
proximity_order <- c('RF-GAP', 'Original', 'OOB', 'PBK', 'RFProxIH', 'RF')


errors <- errors[order(match(Proximity, proximity_order))]

errors[, as.list(coef(lm(test_mean ~ oob_mean))), by = Proximity]
differences <- differences[order(match(Proximity, proximity_order))]


differences <- as.data.frame(differences)
errors     <- as.data.frame(errors)


differences$Dataset <- sub("_", " ", differences$Dataset)
differences[, 'Dataset'] <- str_to_title(differences[, 'Dataset'])

errors$Dataset <- sub("_", " ", errors$Dataset)
errors[, 'Dataset'] <- str_to_title(errors[, 'Dataset'])



palette <- brewer.pal(6, 'Dark2')
display.brewer.pal(6, 'Dark2')

colors <- palette[c(1, 5, 2, 3, 6, 4)]
shapes <- c(17, 22, 24, 23, 21, 25)

g <- ggplot(data = errors,
            aes(x = oob_mean,
                y = test_mean)) +

  geom_point(aes(color = Proximity,
                 shape = Proximity),
                 size  = 4) +

  geom_abline(intercept = 0, slope = 1) +

  geom_errorbar(aes(ymin  = test_mean - test_sd / 5,
                    ymax  = test_mean + test_sd / 5,
                    color = Proximity)) +

  geom_errorbar(aes(xmin  = oob_mean - oob_sd / 5,
                    xmax  = oob_mean + oob_sd / 5,
                    color = Proximity)) +

  scale_shape_manual(values = shapes, breaks = proximity_order) +
  scale_color_manual(values = colors, breaks = proximity_order) +
  xlab('Error Using Training Set') +
  ylab('Test Error') +


  theme(legend.position = c(.85, .25),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        strip.text.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16))

g

ggsave(paste0(fig_path, 'error_scatter.pdf'), plot = g, device = NULL, width = 6, height = 6)

# Regression slopes




b <- ggplot(data = differences[differences$Proximity != 'RF', ],
            aes(y = reorder(Proximity, match(Proximity, rev(proximity_order[1:5]))),
                x = oob_mean)) +
  geom_boxplot(aes(fill = Proximity)) +

  scale_fill_manual(values = colors, breaks = proximity_order) +

  theme(#legend.position = c(.85, .75),
        legend.position = 'none',
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        strip.text.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16)) +

  # xlab('|RF OOB Error - Proximity Train Error|') +
  xlab('Proportion of Non-Matching OOB Predictions') +
  ylab('') +
  xlim(c(0, 0.4))

b


t <- ggplot(data = differences[differences$Proximity != 'RF', ],
            aes(y = reorder(Proximity, match(Proximity, rev(proximity_order[1:5]))),
                x = test_mean)) +
  geom_boxplot(aes(fill = Proximity)) +

  scale_fill_manual(values = colors, breaks = proximity_order[1:5],
                    labels = proximity_order[1:5],
                    limits = proximity_order[1:5]) +

  theme(legend.position = 'none',
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        strip.text.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 16)) +

  # xlab('|RF Test Error - Proximity Test Error|') +
  xlab('Proportion of Non-Matching Test Predictions') +
  ylab('') +
  xlim(c(0, 0.4))

t


boxplots <- ggarrange(b, t, ncol = 1, nrow = 2)
boxplots

ggsave(paste0(fig_path, 'error_boxplots.pdf'), plot = boxplots, device = NULL, width = 7, height = 12)

table_data <- rbind(differences[differences$Proximity != 'RF', ],
                    errors[errors$Proximity == 'RF', ])

colnames(table_data) <- c('Dataset', 'Proximity', 'Train', 'Train_SD', 'Test', 'Test_SD')


#------------------------------------------------------------------------------#
#                          Training Table
#------------------------------------------------------------------------------#

table_train <- as.data.table(table_data[, -which(names(table_data) %in% c('Test', 'Test_SD'))])
table_train[, Train_SD := .(sprintf("(%.2f)", Train_SD))]


table_train_wide <- dcast(table_train, Dataset ~ Proximity,
                          value.var = c('Train', 'Train_SD'))

table_train_wide <- table_train_wide[, c(1, 5, 11, 6, 12, 3, 9, 2, 8, 4, 10, 7, 13)]
colnames(table_train_wide) <- c('Dataset', 'RF', '', 'RF-GAP', '', 'Original', '', 'OOB', '',
                                'PBK', '', 'RFProxIH', '')


hline <- c(-1, 0, nrow(table_train_wide), seq(1, 23, 1))
print(xtable(table_train_wide, align = c('|l|', '|c|', 'c', 'c|', 'c',  'c|', 'c', 'c|',
                                'c', 'c|', 'c', 'c|', 'c', 'r|'),
             caption = '',
             digits = c(0, 0, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2)),
      include.rownames = FALSE,
      hline.after = hline,
      label = 'tab:training_diff')

#------------------------------------------------------------------------------#
#                          Test Table
#------------------------------------------------------------------------------#

table_test <- as.data.table(table_data[, -which(names(table_data) %in% c('Train', 'Train_SD'))])
table_test[, Test_SD := .(sprintf("(%.2f)", Test_SD))]


table_test_wide <- dcast(table_test, Dataset ~ Proximity,
                          value.var = c('Test', 'Test_SD'))

table_test_wide <- table_test_wide[, c(1, 5, 11, 6, 12, 3, 9, 2, 8, 4, 10, 7, 13)]
colnames(table_test_wide) <- c('Dataset', 'RF', '', 'RF-GAP', '', 'Original', '', 'OOB', '',
                                'PBK', '', 'RFProxIH', '')


hline <- c(-1, 0, nrow(table_test_wide), seq(1, 23, 1))
print(xtable(table_test_wide, align = c('|l|', '|c|', 'c', 'c|', 'c',  'c|', 'c', 'c|',
                                         'c', 'c|', 'c', 'c|', 'c', 'r|'),
             caption = '',
             digits = c(0, 0, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2)),
      include.rownames = FALSE,
      hline.after = hline,
      label = 'tab:testing_diff')
