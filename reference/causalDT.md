# Causal Distillation Trees (CDT)

This function implements causal distillation trees (CDT), developed in
Huang et al. (2025). Briefly, CDT is a two-stage procedure that allows
researchers to identify interpretable subgroups with heterogeneous
treatment effects. In the first stage, researchers are free to use any
machine learning model or metalearner to predict the heterogeneous
treatment effects for each individual in the dataset. In the second
stage, CDT “distills” these predicted heterogeneous treatment effects
into interpretable subgroups by fitting an ordinary decision tree using
the predicted heterogeneous treatment effects from the first stage as
the response variable.

## Usage

``` r
causalDT(
  X,
  Y,
  Z,
  W = NULL,
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
  ...
)
```

## Arguments

- X:

  A tibble, data.frame, or matrix of covariates.

- Y:

  A vector of outcomes.

- Z:

  A vector of treatments.

- W:

  A vector of weights corresponding to treatment propensities.

- holdout_prop:

  Proportion of data to hold out for honest estimation of treatment
  effects. Used only if `holdout_idxs` is NULL.

- holdout_idxs:

  A vector of indices to hold out for honest estimation of treatment
  effects. If NULL, a holdout set of size `holdout_prop` x nrow(X) is
  randomly selected.

- teacher_model:

  Teacher model used to estimate individual-level treatment events.
  Should be either "causal_forest" (default), "bcf", or a function. If
  "causal_forest",
  [`grf::causal_forest()`](https://rdrr.io/pkg/grf/man/causal_forest.html)
  is used as the teacher model. If "bcf",
  [`bcf::bcf()`](https://rdrr.io/pkg/bcf/man/bcf.html) is used as the
  teacher model. Otherwise, the function should take in the named
  arguments `X`, `Y`, `Z`, optionally `W` (corresponding to the
  covariates, outcome, treatment, and propensity weights, respectively),
  and (optional) additional arguments passed to the function via `...`.
  Moreover, the function should return a model object that can be used
  to predict individual-level treatment effects using
  `teacher_predict(teacher_model, x)`.

- teacher_predict:

  Function used to predict individual-level treatment effects from the
  teacher model. Should take in two arguments. as input: the first being
  the model object returned by `teacher_model`, and the second being a
  tibble, data.frame, or matrix of covariates. If `NULL`, the default is
  [`predict()`](https://rdrr.io/r/stats/predict.html).

- student_model:

  Student model used to estimate subgroups of individuals and their
  corresponding estimated treatment effects. Should be either "rpart"
  (default) or a function. If "rpart",
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) is used.
  Otherwise, the function should take in two arguments as input: the
  first being a tibble, data.frame, or matrix of covariates, and the
  second being a vector of predicted individual-level treatment effects.
  Moreover, the function should return a list. At a minimum, this list
  should contain one element named `fit` that is a model object that can
  be used to output the leaf membership indices for each observation via
  `predict(student_model, x, type = 'node')`. In general, we recommend
  using the default "rpart".

- rpart_control:

  A list of control parameters for the `rpart` algorithm. See
  `? rpart.control` for details. Used only if `student_model` is
  "rpart".

- rpart_prune:

  Method for pruning the tree. Default is `"none"`. Options are
  `"none"`, `"min"`, and `"1se"`. If `"min"`, the tree is pruned using
  the complexity threshold which minimizes the cross-validation error.
  If `"1se"`, the tree is pruned using the largest complexity threshold
  which yields a cross-vaidation error within one standard error of the
  minimum. If `"none"`, the tree is not pruned.

- nfolds_crossfit:

  Number of folds in cross-fitting procedure. If `teacher_model` is
  "causal_forest", the default is 1 (no cross-fitting is performed).
  Otherwise, the default is 2 (one fold for training the teacher model
  and one fold for estimating the individual-level treatment effects).

- nreps_crossfit:

  Number of repetitions of the cross-fitting procedure. If
  `teacher_model` is "causal_forest", the default is 1 (no cross-fitting
  is performed). Otherwise, the default is 50.

- B_stability:

  Number of bootstrap samples to use in evaluating stability diagnostics
  (which can be used to select an appropriate teacher model). Default
  is 100. Stability diagnostics are only performed if `student_model` is
  an `rpart` object. If `B_stability` is 0, no stability diagnostics are
  performed. We refer to Huang et al. (2025) for additional details on
  using the stability diagnostic to select the teacher model.

- max_depth_stability:

  Maximum depth of the decision tree used in evaluating stability
  diagnostics. If `NULL`, the default is max(4, max depth of fitted
  student model).

- ...:

  Additional arguments passed to the `teacher_model` function.

## Value

A list with the following elements:

- estimate:

  Estimated subgroup average treatment effects tibble with the following
  columns:

  - leaf_id - Leaf node identifier.

  - subgroup - String representation of the subgroup.

  - estimate - Estimated conditional average treatment effect for the
    subgroup.

  - variance - Asymptotic variance of the estimated conditional average
    treatment effect.

  - .var1 - Sample variance for treated observations in the subgroup.

  - .var0 - Sample variance for control observations in the subgroup.

  - .n1 - Number of treated observations in the subgroup.

  - .n0 - Number of control observations in the subgroup.

  - .sample_idxs - Indices of (holdout) observations in the subgroup.

- student_fit:

  Output of `student_model()`, which can vary. If `student_model` is
  "rpart", the output is a list with the following elements:

  - fit - The fitted student model. An `rpart` model object.

  - tree_info - A data.frame with the tree structure/split information.

  - subgroups - A list of subgroups given by their string
    representation.

  - predictions - Student model predictions for the training
    (non-holdout) data.

- teacher_fit:

  A list of (cross-fitted) teacher model fits.

- teacher_predictions:

  The predicted individual-level treatment effects, averaged across all
  cross-fitted teacher model.

- teacher_predictions_ls:

  A list of predicted individual-level treatment effects from each
  (cross-fitted) teacher model fit.

- crossfit_idxs_ls:

  A list of fold indices used in each cross-fit.

- stability_diagnostics:

  A list of stability diagnostics with the following elements:

  - jaccard_mean - Vector of mean Jaccard similarity index for each tree
    depth. The tree depth is given by the vector index.

  - jaccard_distribution - List of Jaccard similarity indices across all
    bootstraps for each tree depth.

  - bootstrap_predictions - List of mean student model predictions (for
    training (non-holdout) data) across all bootstraps for each tree
    depth.

  - bootstrap_predictions_var - List of variance of student model
    predictions (for training (non-holdout) data) across all bootstraps
    for each tree depth.

  - leaf_ids - List of leaf node identifiers, indicating the leaf
    membership of each training sample in the (original) fitted student
    model.

- holdout_idxs:

  Indices of the holdout set.

## References

Huang, M., Tang, T. M., and Kenney, A. M. (2025). Distilling
heterogeneous treatment effects: Stable subgroup estimation in causal
inference. *arXiv preprint arXiv:2502.07275*.

## Examples

``` r
n <- 50
p <- 3
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
Z <- rbinom(n, 1, 0.5)
Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)

# causal distillation trees using causal forest teacher model
# \donttest{
out <- causalDT(X, Y, Z)
# }
```
