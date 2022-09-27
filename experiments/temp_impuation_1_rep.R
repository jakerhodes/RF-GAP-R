# This file is to impute missing data and test predictions using imputed data

library(data.table)
library(missMethods)
library(rfgap)
library(primes)

imputation_path <- 'experiments/imputation/'


set.seed(0)
primes <- generate_n_primes(10000)
seeds <- sample(primes, 100)

proximity_types <- c('original', 'oob', 'rfproxih')
# proximity_types <- c('rfgap', 'original', 'oob')
# proximity_types <- c('rfproxih')

# seeds <- c(420, 327, 303, 117, 1012)
pcts  <- c(0.05, 0.10, 0.25, 0.50, 0.75)


filenames <- c('waveform')


combined_df <- data.frame(matrix(data = 0, nrow = 0, ncol = 5))
colnames(combined_df) <- c('dataset', 'proximity_type', 'pct_missing', 'seed', 'norm_error')


for (filename in filenames) {

  print(filename)

  data_df_path <- 'experiments/imputation/single_iter_results/'

  # TODO: Add trycatch here. Save df for each dataset. Combine and save all at end



  data <- read.table(paste0('datasets/', filename, '.csv'),
                     sep = ',', header = TRUE, stringsAsFactors = TRUE)

  x <- min_max_scale(data[, -1])
  y <- data[, 1]

  prediction_type <- 'regression'

  if (length(unique(y)) <= 10) {
    y <- as.factor(y)
    prediction_type <- 'classification'
  }


  for (proximity_type in proximity_types) {
    counter <- 1

    print(paste('    ', proximity_type))


    if (file.exists(paste0(data_df_path, filename, '_', proximity_type, '.csv'))) {
      data_df <- as.data.frame(fread(paste0(data_df_path, filename, '_', proximity_type, '.csv')))
    } else {
      data_df <- data.frame(matrix(data = 0, nrow = 0, ncol = 5))
      colnames(data_df) <- c('dataset', 'proximity_type', 'pct_missing', 'seed', 'norm_error')
    }



    if (nrow(data_df) > 1) {
      combined_df <- rbind(combined_df, data_df)
      next
    }

    for (pct in pcts) {

      print(paste0('        ', pct))

      for (seed in seeds) {
        set.seed(seed)

        print(paste0('            ', seed))


        sample_indices <- sample(1:nrow(x), round(.70 * nrow(x)))

        x_train <- x[sample_indices, ]
        y_train <- y[sample_indices]


        x_test <- x[-sample_indices, ]
        y_test <- y[-sample_indices]


        set.seed(seed)
        x_miss <- missMethods::delete_MCAR(x_train, pct,
                                           n_mis_stochastic = FALSE)

        error <- tryCatch({

          rf_impute(x_miss, y_train, n_iters = 1,
                    x_true = x_train,
                    type = proximity_type)$continuous_error[2]

        }, error = function(e) {

          0

        })

        data_df[counter, ] <- c(filename, proximity_type, pct, seed, error)

        counter <- counter + 1
      } # seeds

    } #pcts


    fwrite(data_df, paste0(data_df_path, filename, '_', proximity_type, '.csv'))

    combined_df <- rbind(combined_df, data_df)




  } # proximity_types
} # filenames

fwrite(combined_df, paste0(data_df_path, 'combined_results.csv'))
