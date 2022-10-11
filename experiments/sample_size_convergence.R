library(ggplot2)

sizes <- c(100, 200, 500, 1000, 2000, 5000, 10000)
seeds <- c(420, 327, 303, 117, 1012, 840, 654, 606, 234, 2024)
nvars <- 10

get_error <- function(y_true, y_test) {
  correct <- y_true == y_test
  1 - sum(correct)/length(y_true)
}

proximity_path <- 'experiments/sample_size/proximities/'

errors_df <- data.frame(matrix(0, nrow = 3 * length(sizes) * length(seeds), ncol = 7))
names(errors_df) <- c('Proximity', 'Seed', 'Sample_Size', 'Train_Error', 'Test_Error', 'RF_OOB', 'RF_Test')

counter <- 1
for (i in 1:length(sizes)) {
  size <- sizes[i]
  print(paste0('size: ', size))

  n1 <- size / 2
  n2 <- size / 2

  x <- data.frame(matrix(nrow = n1 + n2, ncol = nvars))
  y <- as.factor(c(rep(0, n1), rep(1, n2)))


  for (seed in seeds) {

    print(paste0('    seed: ', seed))

    set.seed(seed)
    means <- rev(seq(0, 1, 1 / (nvars - 1)))
    for (i in 1:nvars) {
      x[, i] <- c(rnorm(n1, 0, 1), rbind(rnorm(n2, means[i], 1)))
    }


    train_idx <- sample(1:(n1 + n2), round(.7 * (n1 + n2), 1))

    x_train <- x[train_idx, ]
    y_train <- y[train_idx]

    x_test <- x[-train_idx, ]
    y_test <- y[-train_idx]


    rf <- ranger(x = x_train, y = y_train, keep.inbag = TRUE, write.forest = TRUE,
                 oob.error = TRUE, importance = 'permutation', num.trees = 499)

    rf_preds_oob  <- rf$predictions
    rf_preds_test <- predict(rf, data = x_test, seed = seed)$predictions


    if (file.exists(paste0(proximity_path,
                           'size_', size,
                           '_rfgap', '_',
                           '70_pct_train_',
                           'seed', '_',
                           seed, '.csv'))) {

      rfgap <- as.rf_proximities(as.matrix(fread(paste0(proximity_path,
                                                        'size_', size,
                                                        '_rfgap', '_',
                                                        '70_pct_train_',
                                                        'seed', '_',
                                                        seed, '.csv'))))
    } else {



      rfgap <- tryCatch({

        get_proximities(x = x_train, y = y_train, x_test = x_test, rf = rf,
                        type = 'rfgap')



      }, error = function(e) {

        0

      })

      fwrite(rfgap, paste0(proximity_path,
                           'size_', size,
                           '_rfgap', '_',
                           '70_pct_train_',
                           'seed', '_',
                           seed, '.csv'),
             col.names = FALSE)
    }

      if (file.exists(paste0(proximity_path,
                             'size_', size,
                             '_oob', '_',
                             '70_pct_train_',
                             'seed', '_',
                             seed, '.csv'))) {

        oob <- as.rf_proximities(as.matrix(fread(paste0(proximity_path,
                                                          'size_', size,
                                                          '_oob', '_',
                                                          '70_pct_train_',
                                                          'seed', '_',
                                                          seed, '.csv'))))
      } else {



        oob <- tryCatch({

          get_proximities(x = x_train, y = y_train, x_test = x_test, rf = rf,
                          type = 'oob')



        }, error = function(e) {

          0

        })

        fwrite(oob, paste0(proximity_path,
                           'size_', size,
                           '_oob', '_',
                           '70_pct_train_',
                           'seed', '_',
                           seed, '.csv'),
               col.names = FALSE)
      }



        if (file.exists(paste0(proximity_path,
                               'size_', size,
                               '_original', '_',
                               '70_pct_train_',
                               'seed', '_',
                               seed, '.csv'))) {

          original <- as.rf_proximities(as.matrix(fread(paste0(proximity_path,
                                                            'size_', size,
                                                            '_original', '_',
                                                            '70_pct_train_',
                                                            'seed', '_',
                                                            seed, '.csv'))))
        } else {



          original <- tryCatch({

            get_proximities(x = x_train, y = y_train, x_test = x_test,
                            y_test = y_test, rf = rf,
                            type = 'original')



          }, error = function(e) {

            0

          })


          fwrite(original, paste0(proximity_path,
                                  'size_', size,
                                  '_original', '_',
                                  '70_pct_train_',
                                  'seed', '_',
                                  seed, '.csv'),
                 col.names = FALSE)
        }


    rf_test_error        <- get_error(y_test, predict(rf, x_test)$predictions)

    rfgap_preds_train    <- predict(rfgap, y_train, y_test)$train_predictions
    rfgap_preds_test     <- predict(rfgap, y_train, y_test)$test_predictions
    oob_preds_train      <- predict(oob, y_train, y_test)$train_predictions
    oob_preds_test       <- predict(oob, y_train, y_test)$test_predictions
    original_preds_train <- predict(original, y_train, y_test)$train_predictions
    original_preds_test  <- predict(original, y_train, y_test)$test_predictions

    rfgap_diff_train    <- get_error(rfgap_preds_train, rf_preds_oob)
    oob_diff_train      <- get_error(oob_preds_train, rf_preds_oob)
    original_diff_train <-get_error(original_preds_train, rf_preds_oob)

    rfgap_diff_test    <- get_error(rfgap_preds_test, rf_preds_test)
    oob_diff_test      <- get_error(oob_preds_test, rf_preds_test)
    original_diff_test <- get_error(original_preds_test, rf_preds_test)


    row <- 3 * counter - 2

    errors_df[row, 1] <- 'RF-GAP'
    errors_df[row + 1, 1] <- 'OOB'
    errors_df[row + 2, 1] <- 'Original'

    errors_df[row, 2:5]     <- c(seed, size, rfgap_diff_train, rfgap_diff_test)
    errors_df[row + 1, 2:5] <- c(seed, size, oob_diff_train, oob_diff_test)
    errors_df[row + 2, 2:5] <- c(seed, size, original_diff_train, original_diff_test)

    errors_df[row, 6:7] <- c(rf$prediction.error, rf_test_error)
    errors_df[row + 1, 6:7] <- c(rf$prediction.error, rf_test_error)
    errors_df[row + 2, 6:7] <- c(rf$prediction.error, rf_test_error)

    # errors_df


    counter <- counter + 1
  }
}


# TODO: Differenciate between train and test!  Still need to do this!!!
errors_dt <- as.data.table(errors_df)
#
fwrite(errors_dt, 'experiments/sample_size/errors.csv')
#
# aggregate_dt <- errors_dt[, .('Mean' = mean(Error_Difference), 'SD' = sd(Error_Difference)), by = c('Proximity', 'Sample_Size')]
#
# ggplot(data = as.data.frame(aggregate_dt), aes(x = as.factor(Sample_Size), y = Mean, fill = Proximity)) +
#   geom_boxplot()
#
# ggplot(data = errors_df, aes(x = as.factor(Sample_Size), y = Error_Difference, fill = Proximity)) +
#   geom_boxplot()
#
#
# ggplot(data = as.data.frame(aggregate_dt), aes(x = Sample_Size, y = Mean, color = Proximity)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = Mean - SD,
#                 ymax = Mean + SD),
#                 width = 2)


# imp <- importance(rf)
#
# plot(imp)

