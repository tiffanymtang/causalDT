#' Arguments that are shared across functions
#'
#' @name shared_args
#'
#' @param X A tibble, data.frame, or matrix of covariates.
#' @param Y A vector of outcomes.
#' @param y A vector of responses to predict.
#' @param Z A vector of treatments.
#' @param rpart_control A list of control parameters for the `rpart` algorithm.
#'   See `? rpart.control` for details.
#' @param rpart_fit An `rpart` object.
#' @param party_fit A `party` object.
#' @param prune Method for pruning the tree. Default is \code{"none"}. Options
#'   are \code{"none"}, \code{"min"}, and \code{"1se"}. If \code{"min"}, the
#'   tree is pruned using the complexity threshold which minimizes the
#'   cross-validation error. If \code{"1se"}, the tree is pruned using the
#'   largest complexity threshold which yields a cross-vaidation error within
#'   one standard error of the minimum. If \code{"none"}, the tree is not
#'   pruned.
#' @param rpart_prune Method for pruning the tree. Default is \code{"none"}.
#'   Options are \code{"none"}, \code{"min"}, and \code{"1se"}. If \code{"min"},
#'   the tree is pruned using the complexity threshold which minimizes the
#'   cross-validation error. If \code{"1se"}, the tree is pruned using the
#'   largest complexity threshold which yields a cross-vaidation error within
#'   one standard error of the minimum. If \code{"none"}, the tree is not
#'   pruned.
#'
#' @keywords internal
NULL

#' Causal Distillation Trees
#'
#' @description TODO
#'
#' @inheritParams shared_args
#' @param holdout_prop Proportion of data to hold out for honest estimation of
#'   treatment effects. Used only if `holdout_idxs` is NULL.
#' @param holdout_idxs A vector of indices to hold out for honest estimation of
#'   treatment effects. If NULL, a holdout set of size `holdout_prop` x nrow(X)
#'   is randomly selected.
#' @param teacher_model Teacher model used to estimate individual-level
#'   treatment events. Should be either "causal_forest" (default) or a function.
#'   If "causal_forest", \code{grf::causal_forest()} is used as the teacher
#'   model. Otherwise, the function should take in the named arguments
#'   `X`, `Y`, `Z`, (corresponding to the covariates, outcome, and treatment
#'   data, respectively) as well as (optional) additional arguments passed to
#'   the function via `...`. Moreover, the function should return a model object
#'   that can be used to predict individual-level treatment effects using
#'   `teacher_predict(teacher_model, x)`.
#' @param teacher_predict Function used to predict individual-level treatment
#'   effects from the teacher model. Should take in two arguments. as input: the
#'   first being the model object returned by `teacher_model`, and the second
#'   being a tibble, data.frame, or matrix of covariates. If \code{NULL}, the
#'   default is \code{predict()}.
#' @param student_model Student model used to estimate subgroups of individuals
#'   and their corresponding estimated treatment effects. Should be either
#'   "rpart" (default) or a function. If "rpart", \code{rpart::rpart()} is used.
#'   Otherwise, the function should take in two arguments as input: the first
#'   being a tibble, data.frame, or matrix of covariates, and the second being a
#'   vector of predicted individual-level treatment effects. Moreover, the
#'   function should return a list. At a minimum, this list should contain one
#'   element named `fit` that is a model object that can be used to output the
#'   leaf membership indices for each observation via
#'   `predict(student_model, x, type = 'node')`. In general, we recommend
#'   using the default "rpart".
#' @param rpart_control A list of control parameters for the `rpart` algorithm.
#'   See `? rpart.control` for details. Used only if `student_model` is "rpart".
#' @param nfolds_crossfit Number of folds in cross-fitting procedure.
#'   If `teacher_model` is "causal_forest", the default is 1 (no cross-fitting
#'   is performed). Otherwise, the default is 2 (one fold for training the
#'   teacher model and one fold for estimating the individual-level treatment effects).
#' @param nreps_crossfit Number of repetitions of the cross-fitting procedure.
#'   If `teacher_model` is "causal_forest", the default is 1 (no cross-fitting
#'   is performed). Otherwise, the default is 50.
#' @param B_stability Number of bootstrap samples to use in evaluating stability
#'   diagnostics. Default is 100. Stability diagnostics are only performed if
#'   `student_model` is an `rpart` object. If `B_stability` is 0, no stability
#'   diagnostics are performed.
#' @param max_depth_stability Maximum depth of the decision tree used in
#'   evaluating stability diagnostics. If \code{NULL}, the default is
#'   max(4, max depth of fitted student model).
#' @param ... Additional arguments passed to the `teacher_model` function.
#'
#' @returns A list with the following elements:
#' \item{estimate}{Estimated subgroup average treatment effects tibble with the following columns:
#'   \itemize{
#'     \item{leaf_id - Leaf node identifier.}
#'     \item{subgroup - String representation of the subgroup.}
#'     \item{estimate - Estimated conditional average treatment effect for the subgroup.}
#'     \item{variance - Asymptotic variance of the estimated conditional average treatment effect.}
#'     \item{.var1 - Sample variance for treated observations in the subgroup.}
#'     \item{.var0 - Sample variance for control observations in the subgroup.}
#'     \item{.n1 - Number of treated observations in the subgroup.}
#'     \item{.n0 - Number of control observations in the subgroup.}
#'     \item{.sample_idxs - Indices of (holdout) observations in the subgroup.}
#'   }
#' }
#' \item{student_fit}{Output of `student_model()`, which can vary. If
#'   `student_model` is "rpart", the output is a list with the following elements:
#'   \itemize{
#'     \item{fit - The fitted student model. An `rpart` model object.}
#'     \item{tree_info - A data.frame with the tree structure/split information.}
#'     \item{subgroups - A list of subgroups given by their string representation.}
#'     \item{predictions - Student model predictions for the training (non-holdout) data.}
#'   }
#' }
#' \item{teacher_fit}{A list of (cross-fitted) teacher model fits.}
#' \item{teacher_predictions}{The predicted individual-level treatment effects, averaged across all cross-fitted teacher model.}
#' \item{teacher_predictions_ls}{A list of predicted individual-level treatment effects from each (cross-fitted) teacher model fit.}
#' \item{crossfit_idxs_ls}{A list of fold indices used in each cross-fit.}
#' \item{stability_diagnostics}{A list of stability diagnostics with the following elements:
#'   \itemize{
#'     \item{jaccard_mean - Vector of mean Jaccard similarity index for each tree depth. The tree depth is given by the vector index.}
#'     \item{jaccard_distribution - List of Jaccard similarity indices across all bootstraps for each tree depth.}
#'     \item{bootstrap_predictions - List of mean student model predictions (for training (non-holdout) data) across all bootstraps for each tree depth.}
#'     \item{bootstrap_predictions_var - List of variance of student model predictions (for training (non-holdout) data) across all bootstraps for each tree depth.}
#'     \item{leaf_ids - List of leaf node identifiers, indicating the leaf membership of each training sample in the (original) fitted student model.}
#'   }
#' }
#' \item{holdout_idxs}{Indices of the holdout set.}
#'
#' @examples
#' n <- 100
#' p <- 5
#' X <- matrix(rnorm(n * p), nrow = n, ncol = p)
#' Z <- rbinom(n, 1, 0.5)
#' Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)
#'
#' # causal distillation trees using causal forest teacher model
#' out <- causalDT(X, Y, Z)
#'
#' \dontrun{
#' # causal distillation trees using rboost teacher model
#' out <- causalDT(X, Y, Z, teacher_model = rboost)
#' }
#'
#' @export
causalDT <- function(X, Y, Z,
                     holdout_prop = 0.3,
                     holdout_idxs = NULL,
                     teacher_model = "causal_forest",
                     teacher_predict = NULL,
                     student_model = "rpart",
                     rpart_control = NULL,
                     rpart_prune = c("none", "min", "1se"),
                     nfolds_crossfit = NULL,
                     nreps_crossfit = NULL,
                     B_stability = 100,
                     max_depth_stability = NULL,
                     ...) {

  rpart_prune <- match.arg(rpart_prune)

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
    if (is.null(nfolds_crossfit)) {
      nfolds_crossfit <- 1
    }
    if (is.null(nreps_crossfit)) {
      nreps_crossfit <- 1
    }
  } else if (!is.function(teacher_model)) {
    stop("`teacher_model` must be a function or 'causal_forest'.")
  }
  if (identical(student_model, "rpart")) {
    student_model <- purrr::partial(
      student_rpart,
      rpart_control = rpart_control,
      prune = rpart_prune
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
