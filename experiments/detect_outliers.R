#--------------------------------------------------------#
#                  Loading Libraries
#--------------------------------------------------------#
library(rfgap)
library(data.table)
#--------------------------------------------------------#
#                   Data Wrangling
#--------------------------------------------------------#
seeds <- c(420, 327, 303, 117, 1012)

num.trees      <- 500
data_path      <- 'datasets/'
outlier_path   <- 'experiments/outlier_measures/'
proximity_path <- 'experiments/proximities/'

filenames <- c('balance_scale', 'banknote',
                'breast_cancer', 'car', 'diabetes', 'ecoli', 'glass',
                'heart_disease', 'hill_valley', 'ionosphere', 'iris', 'liver',
                'lymphography', 'parkinsons', 'seeds', 'sonar','tic-tac-toe',
                'titanic', 'wine', 'optdigits', 'waveform', 'rnaSeq')

types <- c('rfgap', 'oob', 'original', 'rfproxih', 'pbk')


for (filename in filenames) {

  print(filename)

  # if (file.exists(paste0(outlier_path, filename, '_', 'pbk',
  #                        '_seed_', as.character(seed), '.csv'))) {
  #   next
  # }

  data <- tryCatch({fread(paste0(data_path, filename, '.csv'),
                           header = TRUE, sep = ',')},
                    error = function(e) {print(paste0('not read: ', filename))
                    })

  if (!is.data.frame(data)) {
    next
  }

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

  #--------------------------------------------------------#
  #               Running Outlier Detection
  #--------------------------------------------------------#

  for (type in types) {

    for (seed in seeds) {

      prox <- tryCatch({

        as.rf_proximities(as.matrix(fread(paste0(proximity_path,
                                                 filename, '_',
                                                 type, '_',
                                                 'seed', '_',
                                                 seed, '.csv'))))
      }, error = function(e) {
        0
      })

      if (sum(prox) == 0) {
        next
      }


      outlier_measures <- rf_outliers(prox, y, type = type)

      fwrite(as.data.frame(outlier_measures), paste0(outlier_path, filename, '_', type,
                                      '_seed_', as.character(seed), '.csv'),
             row.names = FALSE,
             col.names = FALSE,
             sep = ',')

    }
  }
}
