#--------------------------------------------------------------#
#                  Additional helper functions
#--------------------------------------------------------------#

get_mode <- function(v) {
  uniqv <- as.matrix(unique(v[!is.na(v)]))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

count_unique <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  length(uniqv)
}

min_max_scale <- function(x) {

  factor_cols <- sapply(as.data.frame(x), function(x) {
    is.factor(x)
  })

  if (sum(factor_cols) > 0) {
    warning('Only numeric columns will be affected')
  }


  denom <- (max(x[!factor_cols]) - min(x[!factor_cols]))
  if (denom == 0) {
    denom <- 1
  }

  x[!factor_cols] <- (x[!factor_cols] - min(x[!factor_cols])) / denom

  return(x)
}


# Scales proximity rows to sum up to 1
sum_to_one <- function(x) {
  row_sum <- rowSums(x)
  x <- x / row_sum
  x[is.nan(x)] <- 0
  x

}

# Scale the proximity rows from 0 to 1
zero_one_scale <- function(x) {
  maxs <- apply(abs(x), 1, max)
  x / maxs
}

# Makes a matrix symmetric
make_symmetric <- function(x) {
  (x + t(x)) / 2
}
