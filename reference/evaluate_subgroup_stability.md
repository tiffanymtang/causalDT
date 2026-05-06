# Subgroup stability diagnostics

This function evaluates the stability of the estimated subgroups from
causal distillation trees (CDT) using the Jaccard subgroup stability
index (SSI), developed in Huang et al. (2025). It is generally
recommended to choose teacher models in CDT that result in the most
stable subgroups, as indicated by high SSI values.

## Usage

``` r
evaluate_subgroup_stability(
  estimator,
  fit,
  X,
  y,
  Z = NULL,
  rpart_control = NULL,
  B = 100,
  max_depth = NULL
)
```

## Arguments

- estimator:

  Function used to estimate subgroups of individuals and their
  corresponding estimated treatment effects. The function should take in
  `X`, `y`, and optionally `Z` (if input is not `NULL`) and return a
  model fit (e.g,. output of `rpart`) that can be coerced into a `party`
  object via `partykit::as_party()`. Typically, `student_rpart` will be
  used as the `estimator`.

- fit:

  Fitted subgroup model (often, the output of `estimator()`). Mainly
  used to determine an appropriate `max_depth` for the stability
  diagnostics. If `fit` is not an `rpart` object, stability diagnostics
  will be skipped.

- X:

  A tibble, data.frame, or matrix of covariates.

- y:

  A vector of responses to predict.

- Z:

  A vector of treatments.

- rpart_control:

  A list of control parameters for the `rpart` algorithm. See
  `? rpart.control` for details.

- B:

  Number of bootstrap samples to use in evaluating stability
  diagnostics. Default is 100.

- max_depth:

  Maximum depth of the tree to consider when evaluating stability
  diagnostics. If `NULL`, the default is max(4, max depth of `fit`).

## Value

A list with the following elements:

- jaccard_mean:

  Vector of mean Jaccard similarity index for each tree depth. The tree
  depth is given by the vector index.

- jaccard_distribution:

  List of Jaccard similarity indices across all bootstraps for each tree
  depth.

- bootstrap_predictions:

  List of mean student model predictions (for training (non-holdout)
  data) across all bootstraps for each tree depth.

- bootstrap_predictions_var:

  List of variance of student model predictions (for training
  (non-holdout) data) across all bootstraps for each tree depth.

- leaf_ids:

  List of leaf node identifiers, indicating the leaf membership of each
  training sample in the (original) fitted student model.

## References

Huang, M., Tang, T. M., and Kenney, A. M. (2025). Distilling
heterogeneous treatment effects: Stable subgroup estimation in causal
inference. *arXiv preprint arXiv:2502.07275*.

## Examples

``` r
# \donttest{
n <- 200
p <- 10
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
Z <- rbinom(n, 1, 0.5)
Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)

# run causal distillation trees without stability diagnostics
out <- causalDT(X, Y, Z, B_stability = 0)
# run stability diagnostics
stability_out <- evaluate_subgroup_stability(
  estimator = student_rpart,
  fit = out$student_fit$fit,
  X = X[-out$holdout_idxs, , drop = FALSE],
  y = out$student_fit$predictions
)
# }
```
