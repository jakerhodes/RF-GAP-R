#--------------------------------------------------------#
#                  Loading Libraries
#--------------------------------------------------------#
library(rfgap)
library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)


# TODO: Account for classification vs. regression
# Plot point sizes based on  regression error
#--------------------------------------------------------#
#                   Data Wrangling
#--------------------------------------------------------#
seed <- 420
set.seed(seed)

data_path <- 'datasets/'
proximity_path <- 'experiments/proximities/'
filename <- 'auto-mpg'

prox_types <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')
names      <- c('RF-GAP','Original', 'OOB', 'PBK', 'RFProxIH')

data <- fread(paste0(data_path, filename, '.csv'), header = TRUE, sep = ',')
data <- as.data.frame(data)

n <- nrow(data)
d <- ncol(data) - 1

n_classes <- length(unique(as.factor(data[, 1])))

if (n_classes > 10){
  y <- data[, 1]
} else {
  y <- as.factor(data[, 1])
}

x <- data[, 2:(d + 1)]

rf <- ranger(x = x, y = y, keep.inbag = TRUE, write.forest = TRUE, seed = seed)
preds <- rf$predictions
correct <- preds == y

Class <- y
Correct <- correct
Correct[Correct == TRUE] <- 'Correct'
Correct[Correct == FALSE] <- 'Incorrect'

plots <- list()
mdss  <- list()

Group <- paste(Class, Correct, sep = ', ')

palette <- brewer.pal(12, 'Paired')
display.brewer.pal(12, 'Paired')

# For rnaSeq
# colors <- palette[c(2, 4, 6, 5, 8, 7, 10)]
# shapes <- c(15, 16, 17, 2, 23, 5, 25)


colors <- palette[c(2, 8, 7, 10)]
shapes <- c(15, 16, 1, 17)
#--------------------------------------------------------#
#               Running MDS Detection
#--------------------------------------------------------#

for (type in prox_types) {

  print(paste0('    ', type))

  prox <- as.rf_proximities(as.matrix(fread(paste0(proximity_path,
                                                   filename, '_',
                                                   type, '_',
                                                   'seed', '_',
                                                   seed, '.csv'))))


  if (file.exists(paste0('experiments/mds/embeddings/',
                         filename, '_',
                         type, '_',
                         'seed', '_',
                         seed, '.csv'))) {


    mds <-  as.rf_mds(fread(paste0('experiments/mds/embeddings/',
                                   filename, '_',
                                   type, '_',
                                   'seed', '_',
                                   seed, '.csv')))

    mdss[[type]] <- mds

  } else {

    mds <- rf_mds(prox = prox, mds_type = 'nonmetric')

    fwrite(mds, paste0('experiments/mds/embeddings/',
                       filename, '_',
                       type, '_',
                       'seed', '_',
                       seed, '.csv'),
           col.names = FALSE
    )

    mdss[[type]] <- mds

  }
}

for (type in prox_types) {


  mds <- mdss[[type]]


  name <- paste(type, 'plot', sep = '_')

  plots[[name]] <- ggplot(data = as.data.frame(mds),
                          aes(x = V1, y = V2,
                              color = Group,
                              fill = Group,
                              shape = Group)) +
    geom_point(size = 1) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_shape_manual(values = shapes) +

    theme(legend.position = 'left',
          legend.text     = element_text(size = 15),
          legend.title    = element_text(size = 17),
          title           = element_text(size = 18),
          axis.title      = element_blank(),
          axis.text       = element_blank(),
          axis.ticks      = element_blank(),
          plot.margin     = margin(0.01, 0.01, 0.01, 0.01, 'cm'),
          panel.border    = element_rect(color = 'black', fill = NA),
          plot.title      = element_text(hjust = 0.5)) +

    ggtitle(label = names[prox_types == type])


  individual_pdf <- paste0('experiments/mds/figs/',
                           filename, '_',
                           type, '_',
                           'non-metric-mds_',
                           'seed', '_',
                           seed, '.pdf')

  ggsave(individual_pdf, width = 5, height = 3, dpi = 600)

}

ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
          common.legend = TRUE,
          nrow = 1) +

  theme(plot.margin = margin(0.01, 0.01, 0.01, 0.01, 'cm'))


combined_pdf <- paste0('experiments/mds/figs/',
                    filename, '_',
                    'non-metric-mds_',
                    'seed', '_',
                    seed, '.pdf')

ggsave(combined_pdf, width = 12, height = 3.25, dpi = 1200)

sort(unique(Group))
