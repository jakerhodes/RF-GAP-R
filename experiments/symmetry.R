library(data.table)

proximity_path <- 'experiments/symmetry/proximities/'
n_trees <- c(100, 500, 1000, 5000, 10000, 50000)
# n_trees <- c(100, 500)
size <- 500
seeds <- c(420, 327, 303, 117, 1012, 840, 654, 606, 234, 2024)
nvars <- 10

n1 <- size / 2
n2 <- size / 2

x <- data.frame(matrix(nrow = n1 + n2, ncol = nvars))
y <- as.factor(c(rep(0, n1), rep(1, n2)))

norms_df <- data.frame(matrix(nrow = length(n_trees) * length(seeds),
                              ncol = 3))

names(norms_df) <- c('n_tree', 'seed', 'norm')
counter <- 1
for (n_tree in n_trees) {
  print(paste0('n_tree: ', n_tree))
  for (seed in seeds) {

    print(paste0('    seed: ', seed))

    set.seed(seed)
    means <- rev(seq(0, 1, 1 / (nvars - 1)))
    for (i in 1:nvars) {
      x[, i] <- c(rnorm(n1, 0, 1), rbind(rnorm(n2, means[i], 1)))
    }
#
#
#     train_idx <- sample(1:(n1 + n2), round(.7 * (n1 + n2), 1))
#
#     x_train <- x[train_idx, ]
#     y_train <- y[train_idx]
#
#     x_test <- x[-train_idx, ]
#     y_test <- y[-train_idx]


    rf <- ranger(x = x, y = y, keep.inbag = TRUE, write.forest = TRUE,
                 oob.error = TRUE, importance = 'permutation',
                 num.trees = n_tree)

    # rf_preds_oob  <- rf$predictions
    # rf_preds_test <- predict(rf, data = x_test, seed = seed)$predictions


    if (file.exists(paste0(proximity_path,
                           'size_', size,
                           '_rfgap', '_',
                           'seed', '_',
                           seed,
                           '_trees_', n_tree,
                           '.csv'))) {

      rfgap <- as.rf_proximities(as.matrix(fread(paste0(proximity_path,
                                                        'size_', size,
                                                        '_rfgap', '_',
                                                        'seed', '_',
                                                        seed,
                                                        '_trees_', n_tree,
                                                        '.csv'))))
    } else {



      rfgap <- tryCatch({

        get_proximities(x = x, y = y, rf = rf,
                        type = 'rfgap')



      }, error = function(e) {

        0

      })

      fwrite(rfgap, paste0(proximity_path,
                           'size_', size,
                           '_rfgap', '_',
                           'seed', '_',
                           seed,
                           '_trees_', n_tree,
                           '.csv'),
             col.names = FALSE)
    }




    norm <- sqrt(sum((rfgap - t(rfgap))^2))

    norms_df[counter, ] <- c(n_tree, seed, norm)
    counter <- counter + 1

  } # seeds
} # n_trees


norms_dt <- as.data.table(norms_df)
mean_norms <- norms_dt[, .('mean' = mean(norm),
                           'sd'   = sd(norm)), by = n_tree]

plot(mean_norms$mean)
