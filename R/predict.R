#--------------------------------------------------------------#
#                Proximity Prediction
#--------------------------------------------------------------#

# TODO: Build a separate function for comparing predictions
# TODO: Make friendly for prediction on a test setsd (Look into how done with knn)
# Perhaps easiest to supply which indices are for test/train examples for
# the full proximity matrix. I DON"T THINK THIS WAS CORRECTLY DONE FOR THE
# PAPER RESULTS FOR OOB and ORIGINAL TYPES!!!

#' A function to make predictions using random forest proximities
#'  (rf_proximities class)
#' @name predict.rf_proximities
#' @param object An rf_proximity class matrix
#' @param y A vector of class labels or a continuous responses
#' @param y_test A supplied test set (optional)
#' @param ... Additional arguments for running ranger::ranger
#' @return A list which contains the predictions and prediction error rate.
#'   If test_idx is supplied, separate test predictions and error rate will
#'   be included in the list (NOT YET IMPLEMENTED).
#' @examples
#' x <- iris[, -5]
#' y <- iris[, 5]
#' prox <- get_proximities(x, y)
#' preds_list <- predict(prox, y)
#' @export

predict.rf_proximities <- function(object, y, y_test = NULL, ...) {

  if (!is.null(y_test) && nrow(object) != length(y) + length(y_test)) {
    stop('The proximity matrix rows must match the length of the training plus
         test labels')
  }

  if (is.null(y_test) && nrow(object) != length(y)) {
    warning('The number of rows of the proximity matrix must match the length
            of y dnly the proximities of the training predictions are being
            calculated.')

    object <- object[1:length(y), 1:length(y)]
  }

  n_classes <- length(unique(as.factor(y)))
  n_train <- length(y)
  n <- n_train

  if (!is.null(y_test)) {
    n_test  <- length(y_test)
    n <- n_train + n_test
  }


  prox <- object

  if (!is.null(y_test)) {
    prox[, (n_train + 1):n] = 0
  }

  prox_scaled <- sum_to_one(prox)


  preds <- rep(0, n)

  if (is.factor(y)) {
    classes <- unique(y)

    label_matrix <- matrix(rep(0, n_classes * n), ncol = n_classes)

    row <- 1
    for (class in classes) {
      label_matrix[y == class, row] <- 1
      row <- row + 1
    }

    class_probabilities <- prox_scaled %*% label_matrix


    preds <- apply(class_probabilities, 1, which.max)

    train_predictions <- classes[preds[1:n_train]]
    train_correct <- train_predictions == y
    train_error <- 1 - sum(train_correct) / n_train


    if (!is.null(y_test)) {
      test_predictions  <- classes[preds[(n_train + 1):n]]
      test_correct <- test_predictions == y_test
      test_error <- 1 - sum(test_correct) / n_test

    }

  } else { # Start of the regression predictions

    # TODO: Make adjustments for test set for regression

    train_predictions <- prox_scaled[1:n_train, 1:n_train] %*% y
    train_error       <- sum((train_predictions - y_train)^2) / n_train


    if (!is.null(y_test)) {
      test_predictions <- prox_scaled[(n_train + 1):n, 1:n_train] %*% y
      test_error <- sum((test_predictions - y_test)^2) / n_test

    }
    ###################################################
  }

  if (!is.null(y_test)) {
    return(list("train_predictions" = train_predictions,
                "train_error"       = train_error,
                "test_predictions"  = test_predictions,
                "test_error"        = test_error
                ))
  }
  return(list("predictions" = train_predictions, "error" = train_error))
}
