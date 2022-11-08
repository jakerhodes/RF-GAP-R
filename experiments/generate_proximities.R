# This file is to run comparisons of proximity-based predictions with random
# forest predictions


proximity_path <- 'experiments/proximities/'


proximity_types <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')
# proximity_types <- c('rfgap', 'original', 'oob')


seeds = c(420, 327, 303, 117, 1012)

# filenames <- c('auto-mpg', 'arrhythmia', 'balance_scale', 'banknote', 'breast_cancer',
#                'car', 'diabetes', 'ecoli', 'glass', 'heart_disease',
#                'hill_valley', 'ionosphere', 'iris', 'liver', 'lymphography',
#                'parkinsons','seeds', 'sonar',
#                'tic-tac-toe', 'titanic', 'wine', 'rnaSeq', 'optdigits', 'waveform')

filenames <- c('optdigits')

for (filename in filenames) {

  print(filename)

  for (seed in seeds) {
    set.seed(seed)

    data <- read.table(paste0('datasets/', filename, '.csv'),
                       sep = ',', header = TRUE)

    x <- data[, -1]
    y <- data[, 1]

    prediction_type <- 'regression'

    if (length(unique(y)) <= 10) {
      y <- as.factor(y)
      prediction_type <- 'classification'
    }

    rf <- ranger(x = x, y = y, write.forest = TRUE, keep.inbag = TRUE,
                 seed = seed)


    for (proximity_type in proximity_types) {

      print(paste('    ', proximity_type, 'seed: ', seed))

      if (file.exists(paste0(proximity_path,
                             filename, '_',
                             proximity_type, '_',
                             'seed', '_',
                             seed, '.csv'))) {

        prox <- as.rf_proximities(as.matrix(fread(paste0(proximity_path,
                                                         filename, '_',
                                                         proximity_type, '_',
                                                         'seed', '_',
                                                         seed, '.csv'))))
      } else {



        prox <- tryCatch({

          get_proximities(x = x, y = y, rf = rf, type = proximity_type)

        }, error = function(e) {

          0

        })


        if (sum(prox) == 0) {

          print(paste('failed', filename, 'type:',
                      proximity_type, 'seed: ', seed))

          next
        }

        fwrite(prox, paste0(proximity_path,
                            filename, '_',
                            proximity_type, '_',
                            'seed', '_',
                            seed, '.csv'),
               col.names = FALSE
        )
      }


    }

  }
}
