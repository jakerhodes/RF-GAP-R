# This file is to run comparisons of proximity-based predictions with random
# forest predictions


proximity_path <- 'experiments/proximities_split/'


proximity_types <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')
proximity_names <- c('RF-GAP', 'Original', 'OOB', 'PBK', 'RFProxIH')
proximity_diffs <- paste(proximity_types, 'diff', sep = '_')
# proximity_types <- c('rfgap', 'original', 'oob')


seeds = c(420, 327, 303, 117, 1012)

filenames <- c('auto-mpg', 'arrhythmia', 'balance_scale', 'banknote', 'breast_cancer',
               'car', 'diabetes', 'ecoli', 'glass', 'heart_disease',
               'hill_valley', 'ionosphere', 'iris', 'liver', 'lymphography',
               'parkinsons', 'seeds', 'sonar',
               'tic-tac-toe', 'titanic', 'wine', 'rnaSeq', 'optdigits', 'waveform')


# filenames <- c('auto-mpg')

combined_df <- data.frame(matrix(nrow = 0,
                                 ncol = 2 * length(proximity_types) + 4))

colnames(combined_df) <- c('Dataset', 'Seed', 'Prediction', 'RF', proximity_names, proximity_diffs)

for (filename in filenames) {

  print(filename)
  proximity_predictions <- list()
  prediction_matches <- data.frame(matrix(nrow = length(seeds) * 2,
                                          ncol = 2 * length(proximity_types) + 4))

  colnames(prediction_matches) <- c('Dataset', 'Seed', 'Prediction', 'RF', proximity_types, proximity_diffs)


  counter <- 1
  for (seed in seeds) {

    print(paste0('    ', seed))
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

    if (prediction_type == 'regression') {
      y <- min_max_scale(y)
    }


    sample_indices <- sample(1:nrow(x), round(.70 * nrow(x)))

    x_train <- x[sample_indices, ]
    y_train <- y[sample_indices]

    x_test <- x[-sample_indices, ]
    y_test <- y[-sample_indices]

    rf <- ranger(x = x_train, y = y_train, write.forest = TRUE, keep.inbag = TRUE,
                 seed = seed)

    rf_oob_predictions <- rf$predictions
    rf_train_predictions <- predict(rf, x_train, seed = seed)$predictions
    rf_test_predictions <- predict(rf, x_test, seed = seed)$predictions


    for (proximity_type in proximity_types) {

      print(paste0('        ', proximity_type))

      if (file.exists(paste0(proximity_path,
                             filename, '_',
                             proximity_type, '_',
                             '70_pct_train_',
                             'seed', '_',
                             seed, '.csv'))) {

        prox <- as.rf_proximities(as.matrix(fread(paste0(proximity_path,
                                                         filename, '_',
                                                         proximity_type, '_',
                                                         '70_pct_train_',
                                                         'seed', '_',
                                                         seed, '.csv'))))
      } else {



        prox <- tryCatch({

          get_proximities(x = x_train, y = y_train, x_test = x_test,
                                y_test = y_test, rf = rf,
                                type = proximity_type)

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
                            '70_pct_train_',
                            'seed', '_',
                            seed, '.csv'),
               col.names = FALSE
        )
      }


      proximity_predictions[[proximity_type]][[as.character(seed)]] <- predict(prox, y_train, y_test)

      # RF OOB error
      prediction_matches[counter, 'RF'] <- rf$prediction.error


      if (prediction_type == 'classification') {

        # RF Test errors

        prediction_matches[counter + length(seeds), 'RF'] <- 1 - sum(rf_test_predictions == y_test) / length(y_test)


        prediction_matches[counter, proximity_type] <- proximity_predictions[[proximity_type]][[as.character(seed)]]$train_error
        prediction_matches[counter + length(seeds), proximity_type] <- proximity_predictions[[proximity_type]][[as.character(seed)]]$test_error

        # Difference in training error (OOB)
        prediction_matches[counter, paste0(proximity_type, '_diff')] <- 1 - sum(rf$predictions == proximity_predictions[[proximity_type]][[as.character(seed)]]$train_predictions) / length(y_train)

        # Difference in test error
        prediction_matches[counter + length(seeds), paste0(proximity_type, '_diff')] <- 1 - sum(rf_test_predictions == proximity_predictions[[proximity_type]][[as.character(seed)]]$test_predictions) / length(y_test)


      } else {

        # RF Test Error
        prediction_matches[counter + length(seeds), 'RF'] <- sum(rf_test_predictions - y_test)^2 / length(y_test)

        # Difference in training error (OOB)
        prediction_matches[counter, paste0(proximity_type, '_diff')] <- sum(abs(rf$predictions - proximity_predictions[[proximity_type]][[as.character(seed)]]$train_predictions)) / length(y_train)

        # Difference in test error
        prediction_matches[counter + length(seeds), paste0(proximity_type, '_diff')] <- sum(abs(rf_test_predictions - proximity_predictions[[proximity_type]][[as.character(seed)]]$test_predictions)) / length(y_test)


        # Test and Train error
        prediction_matches[counter, proximity_type] <- proximity_predictions[[proximity_type]][[as.character(seed)]]$train_error
        prediction_matches[counter + length(seeds), proximity_type] <- proximity_predictions[[proximity_type]][[as.character(seed)]]$test_error
      }


      prediction_matches[c(counter, counter + length(seeds)), 'Dataset'] <- filename
      prediction_matches[counter, 'Prediction'] <- 'oob'
      prediction_matches[counter + length(seeds), 'Prediction'] <- 'test'
      prediction_matches[c(counter, counter + length(seeds)), 'Seed'] <- seed



    }
    counter <- counter + 1
  }
  colnames(prediction_matches) <- c('Dataset', 'Seed', 'Prediction', 'RF', proximity_names, proximity_diffs)
  combined_df <- rbind(combined_df, prediction_matches)
}

combined_dt <- as.data.table(combined_df)

means <- combined_dt[, lapply(.SD, mean), by = .(Dataset, Prediction)]
means[, 'statistic' := 'mean']

sds <- combined_dt[, lapply(.SD, sd), by = .(Dataset, Prediction)]
sds[, 'statistic' := 'sd']

aggregated_stats <- rbind(means, sds)
stats_long <- melt(aggregated_stats, id.vars = c('Dataset', 'Prediction', 'statistic'), measure.vars = c('RF', proximity_names, proximity_diffs),
                   variable.name = 'Proximity')

stats_wide <- dcast(stats_long, Dataset + Proximity ~ ...)

errors <- stats_wide[Proximity %in% c('RF', proximity_names)]
differences <- stats_wide[Proximity %in% c('RF', proximity_diffs)]
#
data.table::fwrite(errors, paste0(proximity_path, 'aggregated_errors.csv'), na = 'NA')
data.table::fwrite(differences, paste0(proximity_path, 'aggregated_differences.csv'), na = 'NA')

