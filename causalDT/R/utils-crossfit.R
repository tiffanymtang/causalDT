crossfit <- function(estimator, X, Y, Z, split_idxs, ...) {
  nfolds <- length(unique(split_idxs))
  fit_ls <- list()
  for (foldid in 1:nfolds) {
    if (nfolds == 1) {
      train_idxs <- rep(TRUE, length(split_idxs))
    } else {
      train_idxs <- split_idxs != foldid
    }
    fit_ls[[foldid]] <- do.call(
      estimator,
      args = c(
        list(
          X = X[train_idxs, , drop = FALSE],
          Y = Y[train_idxs],
          Z = Z[train_idxs]
        ),
        ...
      )
    )
  }
  return(fit_ls)
}


predict_crossfit <- function(fits, X, split_idxs, predict_fun) {
  nfolds <- length(unique(split_idxs))
  if (nfolds > 1) {
    tauhat_ls <- purrr::imap(
      fits,
      function(fit, foldid) {
        c(predict_fun(fit, X[split_idxs == foldid, , drop = FALSE]))
      }
    )
    tauhat <- rep(NA, nrow(X))
    for (foldid in 1:nfolds) {
      tauhat[split_idxs == foldid] <- tauhat_ls[[foldid]]
    }
  } else {
    # return train/OOB predictions if already stored; o/w compute this manually
    tauhat <- tryCatch(
      c(predict_fun(fits[[1]])),
      error = function(e) {
        return(c(predict_fun(fits[[1]], X)))
      }
    )
  }
  return(tauhat)
}
