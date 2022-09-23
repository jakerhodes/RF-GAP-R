# This file is used to compare agreements across proximity-weighted predictions

proximity_path <- 'experiments/proximities_split/'


proximity_types <- c('rfgap', 'original', 'oob', 'pbk', 'rfproxih')
proximity_names <- c('RF-GAP', 'Original', 'OOB', 'PBK', 'RFProxIH')

seeds = c(420, 327, 303, 117, 1012)

# filenames <- c('balance_scale', 'banknote', 'breast_cancer',
#                'car', 'diabetes', 'ecoli', 'glass', 'heart_disease',
#                'hill_valley', 'ionosphere', 'iris', 'liver', 'lymphography',
#                'parkinsons', 'seeds', 'sonar',
#                'tic-tac-toe', 'titanic', 'wine', 'rnaSeq', 'optdigits', 'waveform')

filenames <- c('balance_scale', 'banknote', 'breast_cancer',
               'diabetes', 'glass', 'hill_valley', 'ionosphere', 'iris',
               'lymphography', 'parkinsons', 'seeds', 'sonar',
               'wine', 'rnaSeq', 'optdigits', 'waveform')

permutation <- permutations(2, 5, repeats.allowed = TRUE) - 1
permutation <- permutation[-which(rowSums(permutation) < 2), ]

permutation_reps <- do.call(rbind, replicate(length(seeds) * length(filenames),
                                             permutation, simplify=FALSE))

proportions_df <- data.frame(matrix(nrow = nrow(permutation_reps),
                                    ncol = length(proximity_types) + 3))


proportions_df[, 1:length(proximity_types)] <- permutation_reps
colnames(proportions_df) <- c(proximity_types, 'seed', 'dataset', 'proportion_matches')


proportions_train_df <- proportions_df
perm_counter <- 1
for (filename in filenames) {

  print(filename)
  proximity_predictions <- list()

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

  for (seed in seeds) {

    print(paste0('    ', seed))
    set.seed(seed)




    sample_indices <- sample(1:nrow(x), round(.70 * nrow(x)))

    x_train <- x[sample_indices, ]
    y_train <- y[sample_indices]

    x_test <- x[-sample_indices, ]
    y_test <- y[-sample_indices]


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
        print('Nope, it does note')
      }


      proximity_predictions[[proximity_type]][[as.character(seed)]] <- predict(prox, y_train, y_test)


    } # proximity_types

    for (i in 1:nrow(permutation)) {

      combo <- permutation[i, ]
      prox_to_compare <- c(proximity_types[which(combo == 1)])
      proportions_df[perm_counter, 'seed'] <- seed
      proportions_df[perm_counter, 'dataset'] <- filename


      proportions_train_df[perm_counter, 'seed'] <- seed
      proportions_train_df[perm_counter, 'dataset'] <- filename

      # For test prediction matches
      test_preds_combined <- sapply(prox_to_compare, function(x){proximity_predictions[[x]][[as.character(seed)]]$test_predictions})
      equal <- apply(test_preds_combined, 1, function(x) {length(unique(x)) == 1})
      proportion_matching <- sum(equal) / length(y_test)
      proportions_df[perm_counter, 'proportion_matches'] <- proportion_matching

      # For training prediction matches
      train_preds_combined <- sapply(prox_to_compare, function(x){proximity_predictions[[x]][[as.character(seed)]]$train_predictions})
      equal <- apply(train_preds_combined, 1, function(x) {length(unique(x)) == 1})
      proportion_matching <- sum(equal) / length(y_train)
      proportions_train_df[perm_counter, 'proportion_matches'] <- proportion_matching


      perm_counter <- perm_counter + 1
    }

  } # seeds

} # filenames


proportion_dt <- as.data.table(proportions_df)
fwrite(proportion_dt, 'experiments/proximities_split/aa_proportion_matches_test.csv')

proportion_train_dt <- as.data.table(proportions_train_df)
fwrite(proportion_train_dt, 'experiments/proximities_split/aa_proportion_matches_train.csv')


