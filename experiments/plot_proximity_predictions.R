library(ggplot2)
library(data.table)
library(RColorBrewer)
library(ggpubr)

fig_path <- 'I:/My Drive/github/RF-GAP/experiments/figs/'
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
differences <- differences[order(match(Proximity, proximity_order))]

palette <- brewer.pal(6, 'Dark2')
display.brewer.pal(6, 'Dark2')

colors <- palette[c(1, 5, 2, 3, 6, 4)]
shapes <- c(17, 22, 24, 23, 21, 25)

g <- ggplot(data <- as.data.frame(errors),
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
errors[, as.list(coef(lm(test_mean ~ oob_mean))), by = Proximity]



b <- ggplot(data <- as.data.frame(differences[Proximity != 'RF']),
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

  xlab('|RF OOB Error - Proximity Train Error|') +
  ylab('') +
  xlim(c(0, 0.4))

b


t <- ggplot(data <- as.data.frame(differences[Proximity != 'RF']),
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

  xlab('|RF Test Error - Proximity Test Error|') +
  ylab('') +
  xlim(c(0, 0.4))

t


boxplots <- ggarrange(b, t, ncol = 1, nrow = 2)
boxplots

ggsave(paste0(fig_path, 'error_boxplots.pdf'), plot = boxplots, device = NULL, width = 7, height = 12)
