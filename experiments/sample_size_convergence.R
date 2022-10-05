library(ggplot2)

# TODO: Loop through all sizes
# TODO: Repeat 10 times
# TODO: Plot differences in errors across times
sizes <- c(100, 500, 1000, 5000, 10000)


n1 <- 100
n2 <- 100
nvars <- 10

x <- data.frame(matrix(nrow = n1 + n2, ncol = nvars))
y <- as.factor(c(rep(0, n1), rep(1, n2)))

train_idx <- sample(1:(n1 + n2), round(.7 * (n1 + n2), 1))




set.seed(420)


# Not right means
means <- rev(seq(0, 1, 1 / (nvars - 1)))
for (i in 1:nvars) {
  x[, i] <- c(rnorm(n1, 0, 1), rbind(rnorm(n2, means[i], 1)))
}

x_train <- x[train_idx, ]
y_train <- y[train_idx]

x_test <- x[-train_idx, ]
y_test <- y[-train_idx]

rf <- ranger(x = x_train, y = y_train, keep.inbag = TRUE, write.forest = TRUE,
             oob.error = TRUE, importance = 'permutation')
imp <- importance(rf)

plot(imp)

rf$prediction.error


rfgap <- get_proximities(x = x_train, rf = rf, x_test = x_test)
oob   <- get_proximities(x = x_train, rf = rf, x_test = x_test, type = 'oob')

rfgap_error <- predict(rfgap, y_train, y_test)
oob_error   <- predict(oob, y_train, y_test)


# ggplot(data = x, aes(x = X1, fill = y)) +
#   geom_histogram(bins = 20) +
#   facet_wrap(y, nrow = 2)


