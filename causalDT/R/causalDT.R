#' Causal Distillation Trees
#'
#' @description TODO
#'
#' @param X A tibble, data.frame, or matrix of covariates.
#' @param Y A vector of outcomes.
#' @param Z A vector of treatments.
#' @param holdout_idxs A vector of indices to hold out for honest estimate of
#'   treatment effects. If NULL, a holdout set of size `holdout_prop` x nrow(X)
#'   is randomly selected.
#' @param holdout_prop Proportion of data to hold out for honest estimate of
#'   treatment effects. Used only if `holdout_idxs` is NULL.
#' @param teacher_model A function that takes in the named arguments `X`, `Y`,
#'   `Z`, and (optional) additional arguments and returns a model object that
#'   can be used to predict individual-level treatment effects using
#'   `teacher_predict(teacher_model, data)`.
#' @param rpart_control A list of control parameters for the rpart function.
#'   See `? rpart.control` for details.
#' @param nfolds_crossfit Number of folds for crossfitting.
#'
#' @export
causalDT <- function(X, Y, Z,
                     holdout_idxs = NULL,
                     holdout_prop = 0.3,
                     teacher_model = "causal_forest",
                     teacher_predict = NULL,
                     student_model = "rpart",
                     rpart_control = NULL,
                     nfolds_crossfit = NULL,
                     nreps_crossfit = NULL,
                     B_stability = 100,
                     max_depth_stability = NULL,
                     ...) {

  # initialize output and helper variables
  n <- nrow(X)

  # check input dimensions
  if (n != length(Y) || n != length(Z)) {
    stop("Input dimensions do not match. X, Y, and Z must have the same number of rows.")
  }

  # check input types
  if (identical(teacher_model, "causal_forest")) {
    teacher_model <- causal_forest
    teacher_predict <- predict_causal_forest
    nfolds_crossfit <- 1
    nreps_crossfit <- 1
  } else if (!is.function(teacher_model)) {
    stop("`teacher_model` must be a function or 'causal_forest'.")
  }
  if (identical(student_model, "rpart")) {
    student_model <- purrr::partial(
      student_rpart,
      rpart_control = rpart_control
    )
    student_stability_model <- student_rpart
  } else if (!is.function(student_model)) {
    stop("`student_model` must be a function or 'rpart'.")
  } else {
    student_stability_model <- student_model
  }

  # set defaults
  if (is.null(teacher_predict)) {
    teacher_predict <- predict
  }
  if (is.null(nfolds_crossfit)) {
    nfolds_crossfit <- 2
  }
  if (is.null(nreps_crossfit)) {
    nreps_crossfit <- 50
  }

  # get holdout indices for honest estimation of CATE
  if (is.null(holdout_idxs) && (holdout_prop == 0)) {
    holdout_idxs <- NULL
    X_train <- X
    Y_train <- Y
    Z_train <- Z
    X_est <- X
    Y_est <- Y
    Z_est <- Z
  } else {
    if (is.null(holdout_idxs)) {
      holdout_idxs <- sample(1:n, size = round(holdout_prop * n))
    }
    X_train <- X[-holdout_idxs, , drop = FALSE]
    Y_train <- Y[-holdout_idxs]
    Z_train <- Z[-holdout_idxs]
    X_est <- X[holdout_idxs, , drop = FALSE]
    Y_est <- Y[holdout_idxs]
    Z_est <- Z[holdout_idxs]
  }

  # Step 1: Train teacher model to estimate individual-level treatment effects.
  teacher_fits_ls <- list()
  tauhats_ls <- list()
  split_idxs_ls <- list()
  for (rep in 1:nreps_crossfit) {
    split_idxs <- sample(
      rep(1:nfolds_crossfit, length.out = nrow(X_train)), size = nrow(X_train)
    )
    teacher_fits <- crossfit(
      estimator = teacher_model,
      X = X_train,
      Y = Y_train,
      Z = Z_train,
      split_idxs = split_idxs,
      ...
    )
    tauhat <- predict_crossfit(
      fits = teacher_fits,
      X = X_train,
      split_idxs = split_idxs,
      predict_fun = teacher_predict
    )
    teacher_fits_ls[[rep]] <- teacher_fits
    split_idxs_ls[[rep]] <- split_idxs
    tauhats_ls[[rep]] <- tauhat
  }

  # Step 2: Train student model (decision tree) to predict predicted
  # individual-level treatment effects (i.e., tauhats)
  tauhat <- Reduce(`+`, tauhats_ls) / nreps_crossfit
  student_fit_out <- student_model(X_train, tauhat)

  # Step 2b: Evaluate stability diagnostics
  stability_out <- evaluate_subgroup_stability(
    estimator = student_stability_model,
    fit = student_fit_out$fit,
    X = X_train,
    y = tauhat,
    rpart_control = rpart_control,
    B = B_stability,
    max_depth = max_depth_stability
  )

  # Step 3: Estimate CATEs for subgroups identified by the student model
  group_cates <- estimate_group_cates(
    fit = student_fit_out$fit,
    X = X_est,
    Y = Y_est,
    Z = Z_est
  )

  out <- list(
    estimate = group_cates,
    student_fit = student_fit_out,
    teacher_fit = teacher_fits_ls,
    teacher_predictions = tauhat,
    teacher_predictions_ls = tauhats_ls,
    crossfit_idxs_ls = split_idxs_ls,
    stability_diagnostics = stability_out,
    holdout_idxs = holdout_idxs
  )
  return(out)
}
