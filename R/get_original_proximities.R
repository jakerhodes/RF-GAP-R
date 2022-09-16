get_original_proximities <- function(rf, x, x_test = NULL) {
  leaf_matrix <- stats::predict(rf, x, type = "terminalNodes")$predictions

  # Used if test set is supplied
  if (!is.null(x_test)) {
    leaf_matrix_test <- stats::predict(rf, x_test,
                                       type = "terminalNodes")$predictions

    leaf_matrix <- rbind(leaf_matrix, leaf_matrix_test)
  }

  n <- dim(leaf_matrix)[1]
  num_trees <- dim(leaf_matrix)[2]

  prox <- matrix(rep(0, n * n), nrow = n, ncol = n)

  for (ind in 1:n) {
    for (t in 1:num_trees) {
      index <- leaf_matrix[ind, t]
      matches <- leaf_matrix[, t] == index
      prox[ind, matches] <- prox[ind, matches] + 1
    }
  }
  prox <- prox / num_trees
  return(prox)
}
