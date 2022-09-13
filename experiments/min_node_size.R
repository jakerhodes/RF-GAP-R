


filenames <- c('auto-mpg', 'arrhythmia', 'balance_scale', 'banknote', 'breast_cancer',
               'car', 'diabetes', 'ecoli', 'glass', 'heart_disease',
               'hill_valley', 'ionosphere', 'iris', 'liver', 'lymphography',
               'parkinsons', 'rnaSeq', 'seeds', 'sonar',
               'tic-tac-toe', 'titanic', 'wine')

# filenames <- c('iris', 'sonar')


proximity_path <- 'experiments/min_node_size/proximities/'

node.sizes <- c(1, 5, 10, 20, 50)
seeds = c(420, 327, 303, 117, 1012)



node_df <- data.frame(matrix(nrow = 0, ncol = 7))

colnames(node_df) <- c('dataset', 'seed', 'node_size', 'pct_no_match', 'rf_error', 'rfgap_error', 'error_difference')

for (filename in filenames) {

  dataset_df <- data.frame(matrix(nrow = 0, ncol = 7))

  colnames(dataset_df) <- c('dataset', 'seed', 'node_size', 'pct_no_match', 'rf_error', 'rfgap_error', 'error_difference')

  print(filename)

  data <- read.table(paste0('datasets/', filename, '.csv'),
                     sep = ',', header = TRUE)

  x <- data[, -1]
  y <- data[, 1]
  n <- length(y)

  prediction_type <- 'regression'

  if (length(unique(y)) <= 10) {
    y <- as.factor(y)
    prediction_type <- 'classification'
  }

  counter <- 1

  for (seed in seeds) {

    for (node.size in node.sizes) {
      set.seed(seed)

      proximity_file <- paste0(proximity_path,
                               filename, '_',
                               'rfgap', '_',
                               'seed', '_',
                               seed, '_',
                               'node_size', '_',
                               node.size, '.csv')

      rf <- ranger(x = x, y = y, write.forest = TRUE, keep.inbag = TRUE,
                   seed = seed, min.node.size = node.size)


        if (file.exists(proximity_file)) {

          prox <- as.rf_proximities(as.matrix(fread(proximity_file)))

        } else {



          prox <- tryCatch({

            get_proximities(x = x, y = y, rf = rf, type = 'rfgap')

          }, error = function(e) {

            0

          })


          if (sum(prox) == 0) {

            print(paste('failed', filename, 'type:',
                        proximity_type, 'seed: ', seed))

            next
          }

          fwrite(prox, proximity_file, col.names = FALSE)

        }


      rf_predictions <- rf$predictions
      rf_error <- rf$prediction.error


      rfgap_classification <- predict(prox, y)
      rfgap_predictions <- rfgap_classification$predictions
      rfgap_error <- rfgap_classification$error


      if (prediction_type == 'regression') {

        pct_no_match <- sum(abs(rfgap_predictions - rf_predictions)) / n

      } else {

        pct_no_match <- 1 - sum(rfgap_predictions == rf_predictions) / n

      }


      error_difference <- rf_error - rfgap_error


      dataset_df[counter, ] <- c(filename, seed, node.size, pct_no_match, rf_error, rfgap_error, error_difference)
      counter <- counter + 1

      # TODO: distinguish between regression and classification datasets

    }
  }

  node_df <- rbind(node_df, dataset_df)
}

write.table(node_df, 'experiments/min_node_size/node_size_results.csv',
            sep = ',',
            row.names = FALSE)

