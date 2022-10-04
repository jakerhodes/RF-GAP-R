# This file is to combine imputation results

library(data.table)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(stringr)

imputation_path <- 'experiments/imputation/'
fig_path <- 'experiments/imputation/figs/'

# proximity_types <- c('rfgap', 'original', 'oob')
# proximity_names <- c('RF-GAP', 'Original', 'OOB')

proximity_types <- c('rfgap', 'original', 'oob', 'rfproxih')
proximity_names <- c('RF-GAP', 'Original', 'OOB', 'RFProxIH')


seeds <- c(420, 327, 303, 117, 1012)
pcts  <- c(0.05, 0.10, 0.25, 0.50, 0.75)

# filenames <- c('auto-mpg', 'arrhythmia', 'balance_scale', 'banknote', 'breast_cancer',
#                'car', 'diabetes', 'ecoli', 'glass', 'heart_disease',
#                'hill_valley', 'ionosphere', 'iris', 'liver', 'lymphography',
#                'parkinsons', 'seeds', 'sonar',
#                'tic-tac-toe', 'titanic', 'wine', 'rnaSeq', 'optdigits', 'waveform')


filenames <- c('auto-mpg', 'arrhythmia', 'balance_scale', 'banknote', 'breast_cancer',
               'diabetes', 'glass',
               'hill_valley', 'ionosphere', 'iris', 'lymphography',
               'parkinsons', 'seeds', 'sonar',
               'wine', 'optdigits', 'waveform')


# filenames <- c('optdigits', 'waveform')
# filenames <- c('iris')

col_names <- c('pct', paste0(proximity_types, '_score'), paste0(proximity_types, '_oob'))


# Graphical Parameters
palette <- brewer.pal(6, 'Dark2')
# display.brewer.pal(6, 'Dark2')


colors <- palette[c(2, 5, 1, 6)]
shapes <- c(2, 22, 17, 1)

pct_labs <- paste0('MCAR ', pcts * 100, '%')
names(pct_labs) <- pcts

plots <- list()

for (filename in filenames) {

  print(filename)

  scores <- data.frame(matrix(nrow = length(seeds) * length(pcts), ncol = (length(proximity_types) * 2 + 1)))
  colnames(scores) <- col_names

  # NEED TO START WORKING ON THIS: SAVE AND PLOT ERRORS
  norms <- data.frame(matrix(nrow = length(seeds) * length(pcts) * length(proximity_types), ncol = 14))
  colnames(norms) <- c('seed', 'pct', 'proximity', as.character(seq(0, 10, 1)))

  counter2 <- 1
  for (proximity_type in proximity_types) {

    print(paste0('    ', proximity_type))

    rds_path <- paste0(imputation_path, filename, '_', proximity_type, '.rds')

    if (!file.exists(rds_path)) {
      next
    }


    imputation_results <- readRDS(rds_path)

    counter <- 1
    for (pct in pcts) {

      for (seed in seeds) {

        scores[counter, 'pct'] <- pct

        scores[counter, paste0(proximity_type, '_score')] <- imputation_results[[as.character(pct)]][[as.character(seed)]]$score
        scores[counter, paste0(proximity_type, '_oob')] <- imputation_results[[as.character(pct)]][[as.character(seed)]]$oob_error

        norms[counter2, 'seed'] <- seed
        norms[counter2, 'pct']  <- pct
        norms[counter2, 'proximity'] <- proximity_type
        norms[counter2, 4:14] <- imputation_results[[as.character(pct)]][[as.character(seed)]]$continuous_error
        # norms[counter2, 4:14] <- imputation_results[[as.character(pct)]][[as.character(seed)]]$categorical_error

        counter <- counter + 1
        counter2 <- counter2 + 1


      }
    }
  }

  for (i in 1:length(proximity_types)) {
    norms[norms['proximity'] == proximity_types[i], 'name'] <- proximity_names[i]
  }


  norms_dt <- as.data.table(norms)

  norm_means <- norms_dt[, lapply(.SD, mean), by = list(proximity, pct, name)]
  norm_means[, c('seed') := NULL]


  means_long <- melt(norm_means, id.vars = c('proximity', 'pct', 'name'), value.name = 'mean', variable.name = 'iteration')

  norm_sds <- norms_dt[, lapply(.SD, sd), by = list(proximity, pct, name)]
  norm_sds[, c('seed') := NULL]


  sds_long <- melt(norm_sds, id.vars = c('proximity', 'pct', 'name'), value.name = 'sd', variable.name = 'iteration')


  norms_data <- cbind(means_long, sds_long[, 'sd'])


  g <- ggplot(data = norms_data, aes(x = iteration, y = mean, color = name,
                                     shape = name)) +
    geom_point() +
    geom_pointrange(aes(ymin = mean - sd / sqrt(5),
                        ymax = mean + sd / sqrt(5))) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    xlab('Iteration') +
    ylab(paste(str_to_title(filename), '-', 'Mean Norm')) +
    labs(color = 'Type', shape = 'Type') +
    facet_wrap(~ pct, ncol =
                 length(pcts), labeller = labeller(pct = pct_labs)) +
    theme(legend.position = c(.05, .75),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          strip.text.x = element_text(size = 17),
          axis.title.y = element_text(size = 17),
          axis.title.x = element_text(size = 17),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 16))


  g

  ggsave(paste0(fig_path, filename, '.pdf'), g, device = NULL, width = 16, height = 4)


  h <- ggplot(data = norms_data, aes(x = iteration, y = mean, color = name,
                                     shape = name)) +
    geom_point() +
    geom_pointrange(aes(ymin = mean - sd / sqrt(5),
                        ymax = mean + sd / sqrt(5))) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    xlab('Iteration') +
    ylab(paste(str_to_title(filename), '-', 'Mean Norm')) +
    labs(color = 'Type', shape = 'Type') +
    facet_wrap(~ pct, ncol =
                 length(pcts), labeller = labeller(pct = pct_labs),
               scales = 'free') +
    theme(legend.position = c(.05, .75),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 10),
          # strip.text.x = element_text(size = 17),
          strip.text.x = element_blank(),
          axis.title.y = element_text(size = 17),
          # axis.title.x = element_text(size = 17),
          axis.title.x = element_blank(),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 16))


  h

  # ggsave(paste0(fig_path, filename, '_independent_scale.pdf'), h, device = NULL, width = 16, height = 4)

  plots[[filename]] <- h


}

names(plots) <- gsub('_', ' ', names(plots))
names(plots) <- str_to_title(names(plots))

#------------------------------------------------------------------------------#

ggarrange(plots[[1]] + rremove("x.text") + ylab(names(plots)[1]) +
            theme(strip.text.x = element_text(size = 12)),
          plots[[2]] + rremove("x.text") + ylab(names(plots)[2]),
          plots[[3]] + rremove("x.text") + ylab(names(plots)[3]),
          plots[[4]] + rremove("x.text") + ylab(names(plots)[4]),
          plots[[5]] + rremove("x.text") + ylab(names(plots)[5]),
          plots[[6]] + theme(axis.title.x = element_text(size = 12)) +
            ylab(names(plots)[6]),
          common.legend = TRUE,
          nrow = 6,
          align = 'v',
          label.x = paste0('MCAR ', c(5, 10, 25, 50, 75), '%')) +

  theme(plot.margin = margin(0.0001, 0.0001, 0.0001, 0.0001, 'cm'))

ggsave('experiments/figs/imputation_1-5.pdf', height = 11, width = 10)

#------------------------------------------------------------------------------#

ggarrange(plots[[7]] + rremove("x.text") + ylab(names(plots)[7]) +
            theme(strip.text.x = element_text(size = 12)),
          plots[[8]] + rremove("x.text") + ylab(names(plots)[8]),
          plots[[9]] + rremove("x.text") + ylab(names(plots)[9]),
          plots[[10]] + rremove("x.text") + ylab(names(plots)[10]),
          plots[[11]] + rremove("x.text") + ylab(names(plots)[11]),
          plots[[12]] + theme(axis.title.x = element_text(size = 12)) +
            ylab(names(plots)[12]),
          common.legend = TRUE,
          nrow = 6,
          align = 'v',
          label.x = paste0('MCAR ', c(5, 10, 25, 50, 75), '%')) +

  theme(plot.margin = margin(0.0001, 0.0001, 0.0001, 0.0001, 'cm'))

ggsave('experiments/figs/imputation_6-12.pdf', height = 11, width = 10)

#------------------------------------------------------------------------------#

ggarrange(plots[[13]] + rremove("x.text") + ylab(names(plots)[13]) +
            theme(strip.text.x = element_text(size = 12)),
          plots[[14]] + rremove("x.text") + ylab(names(plots)[14]),
          plots[[15]] + rremove("x.text") + ylab(names(plots)[15]),
          plots[[16]] + rremove("x.text") + ylab(names(plots)[16]),
          plots[[17]] + theme(axis.title.x = element_text(size = 12)) +
            ylab(names(plots)[17]),
          common.legend = TRUE,
          nrow = 6,
          align = 'v',
          label.x = paste0('MCAR ', c(5, 10, 25, 50, 75), '%')) +

  theme(plot.margin = margin(0.0001, 0.0001, 0.0001, 0.0001, 'cm'))

ggsave('experiments/figs/imputation_13-17.pdf', height = 11, width = 10)

