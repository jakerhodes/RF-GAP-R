#--------------------------------------------------------#
#                  Loading Libraries
#--------------------------------------------------------#
library(rfgap)
library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)


#--------------------------------------------------------#
#                   Data Wrangling
#--------------------------------------------------------#
seed <- 327
set.seed(seed)

data_path <- 'datasets/'
proximity_path <- 'experiments/proximities/'
outlier_path   <- 'experiments/outlier_measures/'
filename <- 'wine'

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

outlier_measures <- as.data.frame(fread(paste0(outlier_path,
                                 filename, '_',
                                 'rfgap', '_',
                                 'seed', '_', seed, '.csv'
                                 )))


plots <- list()
mdss  <- list()
outlier_measures_list <- list()

Group <- paste(Class, Correct, sep = ', ')

palette <- brewer.pal(12, 'Paired')
display.brewer.pal(12, 'Paired')

# For rnaSeq
# colors <- palette[c(2, 4, 6, 5, 8, 7, 10)]
# shapes <- c(15, 16, 17, 2, 23, 5, 25)


colors <- palette[c(4, 3, 8, 7, 10)]
shapes <- c(15, 0, 16, 1, 17)
scale  <- seq(2, 5, .01)
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

  outlier_measures <- as.data.frame(fread(paste0(outlier_path, filename, '_', type,
                                           '_seed_', as.character(seed), '.csv'),
                                          ))

  outlier_measures_list[[type]] <- outlier_measures


  if (file.exists(paste0('experiments/outlier_measures/embeddings/',
                         filename, '_',
                         type, '_',
                         'seed', '_',
                         seed, '.csv'))) {


    mds <-  as.rf_mds(fread(paste0('experiments/outlier_measures/embeddings/',
                                   filename, '_',
                                   type, '_',
                                   'seed', '_',
                                   seed, '.csv')))

    mdss[[type]] <- mds

  } else {

    mds <- rf_mds(prox = prox, mds_type = 'nonmetric')

    fwrite(mds, paste0('experiments/outlier_measures/embeddings/',
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

  outlier_measures <- outlier_measures_list[[type]]

  scale <- .5 + min_max_scale(outlier_measures) * 3
  scale <- scale$V1


  name <- paste(type, 'plot', sep = '_')

  plots[[name]] <- ggplot(data = as.data.frame(mds),
                          aes(x = V1, y = V2,
                              color = Group,
                              fill = Group,
                              shape = Group)) +

    geom_point(size = scale) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values  = colors) +
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


  individual_pdf <- paste0('experiments/outlier_measures/figs/',
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


combined_pdf <- paste0('experiments/outlier_measures/figs/',
                       filename, '_',
                       'non-metric-mds_',
                       'seed', '_',
                       seed, '.pdf')

ggsave(combined_pdf, width = 12, height = 3.25, dpi = 1200)

sort(unique(Group))
