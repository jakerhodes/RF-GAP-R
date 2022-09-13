# This file is to combine imputation results

library(data.table)
library(ggplot2)
library(stringr)
library(RColorBrewer)

imputation_path <- 'experiments/imputation/'
fig_path <- 'experiments/imputation/figs/'

# proximity_types <- c('rfgap', 'original', 'oob')
# proximity_names <- c('RF-GAP', 'Original', 'OOB')

proximity_types <- c('rfgap', 'original', 'oob', 'rfproxih')
proximity_names <- c('RF-GAP', 'Original', 'OOB', 'RFProxIH')


seeds <- c(420, 327, 303, 117, 1012)
pcts  <- c(0.05, 0.10, 0.25, 0.50, 0.75)

filenames <- c('auto-mpg', 'arrhythmia', 'balance_scale', 'banknote', 'breast_cancer',
               'car', 'diabetes', 'ecoli', 'glass', 'heart_disease',
               'hill_valley', 'ionosphere', 'iris', 'liver', 'lymphography',
               'parkinsons', 'seeds', 'sonar',
               'tic-tac-toe', 'titanic', 'wine', 'rnaSeq', 'optdigits', 'waveform')


# filenames <- c('iris')

col_names <- c('pct', paste0(proximity_types, '_score'), paste0(proximity_types, '_oob'))


# Graphical Parameters
palette <- brewer.pal(6, 'Dark2')
# display.brewer.pal(6, 'Dark2')


colors <- palette[c(2, 4, 1, 5, 6)]
shapes <- c(21, 22, 17, 23, 25)

pct_labs <- paste0('MCAR ', pcts * 100, '%')
names(pct_labs) <- pcts


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


}


# for (pct in pcts) {
#   boxplot(scores[scores['pct'] == pct, 2:4])
# }

# long <- melt(setDT(scores), id.vars = 'pct', measure.vars = c(2, 3, 4), variable.name = "proximity_type")
#
#
# anova <- aov(value ~ proximity_type, long)
#
# summary(anova)
#
#
# anova_pct <- aov(value ~ pct, long)
# summary(anova_pct)
# boxplot(scores[, ])

# plot(1:11, norms[1, 4:14])


