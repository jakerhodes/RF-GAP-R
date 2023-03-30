#' This function generates random forest proximities of a variety of types.
#' THIS FUNCTION IS NOT CURRENLY IMPLEMENTED
#'
#' @name rf_upsample
#' @param x A dataframe or matrix of data. Rows (n) are observations,
#'   columns (d) are variables.
#' @param y The labels corresponding to x. If categorical, should be of type
#'   'factor'.
#' @param rf A forest object of s3 class 'ranger'. Note: rf requires
#'   write.forest = TRUE and keep.inbag = TRUE.
#' @param prox a precomputed random forest proximity matrix
#' @param n_samples How many samples should be generated?
#' @param x_test An (optional) test dataset (dataframe or matrix)
#' @param type The type of proximities to be obtained.
#'  Options are rfgap (default), original, oob, pbk, or rfproxih.
#' @param class_weights how the number of samples should be weighted. Currently,
#'  only 'uniform' is supported
#' @param ... Additional argument options for ranger(). (If rf is not supplied).
#' @return A matrix of proximity values.
#' @examples
#'
#' x <- iris[, -5]
#' y <- iris[, 5]
#'

#' @export


rf_upsample <- function(x, y, rf = NULL, type = 'rfgap', x_test = NULL,
                        prox = NULL, n_samples = 2 * nrow(x),
                        class_weights = 'uniform', ...) {

  n_classes <- length(unique(y))

  if (class_weights == 'uniform'){
    class_weights = rep(1 / n_classes, n_classes)
  }


}
