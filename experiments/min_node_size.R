

filenames <- c('auto-mpg', 'arrhythmia', 'balance_scale', 'banknote',
               'breast_cancer', 'car', 'diabetes', 'ecoli', 'glass',
               'heart_disease', 'hill_valley', 'ionosphere', 'iris', 'liver',
               'lymphography', 'parkinsons', 'seeds', 'sonar', 'tic-tac-toe',
               'titanic', 'wine', 'rnaSeq', 'optdigits')


proximity_path <- 'experiments/min_node_size/proximities/'

node.sizes <- c(1, 5, 10, 20, 50)
seeds = c(420, 327, 303, 117, 1012)
proximity_types <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')


node_df <- data.frame(matrix(data = 0, nrow = 0, ncol = 8))

colnames(node_df) <- c('dataset', 'seed', 'node_size', 'pct_no_match', 'rf_error', 'prox_error', 'error_difference', 'proximity_type')

for (filename in filenames) {

  dataset_df <- data.frame(matrix(nrow = 0, ncol = 8))

  colnames(dataset_df) <- c('dataset', 'seed', 'node_size', 'pct_no_match', 'rf_error', 'prox_error', 'error_difference', 'proximity_type')

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

  for (proximity_type in proximity_types) {

    print(paste0('    ', proximity_type))

    for (seed in seeds) {

      print(paste0('        ', seed))

      for (node.size in node.sizes) {
        set.seed(seed)

        print(paste0('            ', node.size))

        proximity_file <- paste0(proximity_path,
                                 filename, '_',
                                 proximity_type, '_',
                                 'seed', '_',
                                 seed, '_',
                                 'node_size', '_',
                                 node.size, '.csv')



        if (file.exists(proximity_file)) {

          rf <- ranger(x = x, y = y, write.forest = TRUE, keep.inbag = TRUE,
                       seed = seed, min.node.size = node.size)

          prox <- as.rf_proximities(as.matrix(fread(proximity_file)))

        } else {



          prox <- tryCatch({

            rf <- ranger(x = x, y = y, write.forest = TRUE, keep.inbag = TRUE,
                         seed = seed, min.node.size = node.size)

            get_proximities(x = x, y = y, rf = rf, type = proximity_type)

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


        proximity_classification <- predict(prox, y)
        proximity_predictions <- proximity_classification$predictions
        prox_error <- proximity_classification$error


        if (prediction_type == 'regression') {

          pct_no_match <- sum(abs(proximity_predictions - rf_predictions)) / n

        } else {

          pct_no_match <- 1 - sum(proximity_predictions == rf_predictions) / n

        }


        error_difference <- rf_error - prox_error


        dataset_df[counter, ] <- c(filename, seed, node.size, pct_no_match, rf_error, prox_error, error_difference, proximity_type)
        counter <- counter + 1

        # TODO: distinguish between regression and classification datasets

      }
    }

    node_df <- rbind(node_df, dataset_df)
  }
  node_df$node_size <- as.numeric(node_df$node_size)
  node_df$pct_no_match <- as.numeric(node_df$pct_no_match)
  node_df$rf_error <- as.numeric(node_df$rf_error)
  node_df$prox_error <- as.numeric(node_df$prox_error)
  node_df$error_difference <- as.numeric(node_df$error_difference)

  write.table(node_df, 'experiments/min_node_size/node_size_results.csv',
              sep = ',',
              row.names = FALSE)

}






