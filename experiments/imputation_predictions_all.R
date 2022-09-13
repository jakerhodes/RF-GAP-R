# This file is to impute missing data and test predictions using imputed data

library(data.table)
library(missMethods)

imputation_path <- 'experiments/imputation/'


proximity_types <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')
# proximity_types <- c('rfgap', 'original', 'oob')


seeds <- c(420, 327, 303, 117, 1012)
pcts  <- c(0.05, 0.10, 0.25, 0.50, 0.75)

# seeds <- c(420, 237)
# pcts  <- c(0.05, .10)

filenames <- c('auto-mpg', 'arrhythmia', 'balance_scale', 'banknote', 'breast_cancer',
               'car', 'diabetes', 'ecoli', 'glass', 'heart_disease',
               'hill_valley', 'ionosphere', 'iris', 'liver', 'lymphography',
               'parkinsons', 'rnaSeq', 'seeds', 'sonar',
               'tic-tac-toe', 'titanic', 'wine', 'optdigits', 'waveform')


for (filename in filenames) {

  print(filename)
  results <- list()

  if (file.exists(paste0(imputation_path,
                          filename, '.rds'))) {
    next

  }

  for (seed in seeds) {
    set.seed(seed)

    data <- read.table(paste0('datasets/', filename, '.csv'),
                       sep = ',', header = TRUE, stringsAsFactors = TRUE)

    # Scale the data so as to be on the same scale (what about categorical?)
    x <- min_max_scale(data[, -1])
    y <- data[, 1]

    prediction_type <- 'regression'

    if (length(unique(y)) <= 10) {
      y <- as.factor(y)
      prediction_type <- 'classification'
    }


    sample_indices <- sample(1:nrow(x), round(.70 * nrow(x)))

    x_train <- x[sample_indices, ]
    y_train <- y[sample_indices]


    x_test <- x[-sample_indices, ]
    y_test <- y[-sample_indices]


    for (proximity_type in proximity_types) {

      print(paste('    ', proximity_type))

      for (pct in pcts) {

        print(paste('        ', pct, 'seed:', seed))

        set.seed(seed)
        x_miss <- missMethods::delete_MCAR(x_train, pct,
                                           n_mis_stochastic = FALSE)

        temporary_results <- tryCatch({

          rf_impute(x_miss, y_train, n_iters = 10,
                                       x_true = x_train,
                                       type = proximity_type)
        }, error = function(e) {

          0

          })

        if (class(temporary_results) != 'list') {
          next
        }


        results[[filename]][[proximity_type]][[as.character(pct)]][[as.character(seed)]] <- temporary_results
        results[[filename]][[proximity_type]][[as.character(pct)]][[as.character(seed)]][['true_data']] <- x_train
        results[[filename]][[proximity_type]][[as.character(pct)]][[as.character(seed)]][['missing_data']] <- x_miss

        rf <- ranger(x = temporary_results$x_imp, y = y_train, oob.error = TRUE,
                     seed = seed)


        preds <- predict(rf, x_test, seed = seed)$predictions

        if (prediction_type == 'classification') {
          score <- 1 - (sum(preds == y_test) / length(y_test))
        } else {
          score <- sum((preds - y_test)^2) / length(y_test)
        }

        results[[filename]][[proximity_type]][[as.character(pct)]][[as.character(seed)]][['oob_error']] <- rf$prediction.error
        results[[filename]][[proximity_type]][[as.character(pct)]][[as.character(seed)]][['score']] <- score


      }
    }
  }

  saveRDS(results, file = paste0(imputation_path,
                                 filename, '.rds'))
}
