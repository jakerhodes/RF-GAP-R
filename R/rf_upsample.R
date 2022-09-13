


#' This function generates random forest proximities of a variety of types.
#'
#' @name rf_upsample
#' @param x A dataframe or matrix of data. Rows (n) are observations,
#'   columns (d) are variables.
#' @param y The labels corresponding to x. If categorical, should be of type
#'   'factor'.
#' @param rf A forest object of s3 class 'ranger'. Note: rf requires
#'   write.forest = TRUE and keep.inbag = TRUE.
#' @param x_test An (optional) test dataset (dataframe or matrix)
#' @param type The type of proximities to be obtained.
#'  Options are rfgap (default), original, oob, pbk, or rfproxih.
#' @param ... Additional argument options for ranger(). (If rf is not supplied).
#' @return A matrix of proximity values.
#' @examples
#'
#' x <- iris[, -5]
#' y <- iris[, 5]
#'

#' @export


rf_upsample <- function(x, y, rf = NULL, prox = NULL, n_samples = 2 * nrow(x),
                        class_weights = 'uniform') {

  n_classes <- length(unique(y))

  if (class_weights == 'uniform'){
    class_weights = rep(1 / n_classes, n_classes)
  }


}


################################################################################
# Iris Example
################################################################################
# x <- iris[, 1:4]
# y <- iris[, 5]

# rf <- ranger(x = x, y = y, write.forest = TRUE, keep.inbag = TRUE)
# prox <- get_proximities(x, rf = rf, type = 'rfgap')


# samp <- sample(1:150, 10, replace = TRUE)
# ext <- rowSums(t(x[samp, ]) %*% prox[samp, ]) / length(samp)



# nobs <- 1000
# nsamps <- 3
# exts <- matrix(rep(0, 4 * 3 * nobs), ncol = 4)
# # exts <- matrix(rep(0, 4 * nobs), ncol = 4)
# # plot(x[, 1], x[, 3])

# for (i in 1:nobs) {
#   samp <- sample(which(y == 'setosa'), nsamps, replace = TRUE)
#   ext <- rowSums(t(x[samp, ]) %*% prox[samp, ]) / length(samp)
#   exts[i, ] <- ext
# }

# for (i in 1:nobs) {
#   samp <- sample(which(y == 'virginica'), nsamps, replace = TRUE)
#   ext <- rowSums(t(x[samp, ]) %*% prox[samp, ]) / length(samp)
#   exts[i + nobs, ] <- ext
# }

# for (i in 1:nobs) {
#   samp <- sample(which(y == 'versicolor'), nsamps, replace = TRUE)
#   ext <- rowSums(t(x[samp, ]) %*% prox[samp, ]) / length(samp)
#   exts[i + 2 * nobs, ] <- ext
# }

# exts.df <- data.frame('Sepal.Length' = jitter(exts[, 1]),
#                       'Sepal.Width'  = jitter(exts[, 2]),
#                       'Petal.Length' = jitter(exts[, 3]),
#                       'Petal.Width'  = jitter(exts[, 4]))

# plot(exts.df)

# pairs(x)



#################################################################################
# MNIST EXAMPLE
#################################################################################

# data <- as.data.frame(fread('datasets/mnist_test.csv'))
#
#
# x <- data[, -1]
# y <- data[, 1]
#
# rf <- ranger(x = x, y = y, write.forest = TRUE, keep.inbag = TRUE)
# prox <- get_proximities(x, rf = rf, type = 'rfgap')
#
#
# samp <- sample(which(y == 7), 3, replace = TRUE)
# ext <- rowSums(t(x[samp, ]) %*% prox[samp, ]) / length(samp)
# ext_im <- matrix(ext, nrow = 28, byrow = TRUE)
# imagesc(ext_im)
