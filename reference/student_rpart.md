# Rpart wrapper for causal distillation trees.

This function is a wrapper around
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) that can be
easily used as a student model in the causal distillation tree
framework.

## Usage

``` r
student_rpart(
  X,
  y,
  method = "anova",
  rpart_control = NULL,
  prune = c("none", "min", "1se"),
  fit_only = FALSE
)
```

## Arguments

- X:

  A tibble, data.frame, or matrix of covariates.

- y:

  A vector of responses to predict.

- method:

  Same as `method` argument in
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html). Default
  is `"anova"`. See
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) for more
  details.

- rpart_control:

  A list of control parameters for the `rpart` algorithm. See
  `? rpart.control` for details.

- prune:

  Method for pruning the tree. Default is `"none"`. Options are
  `"none"`, `"min"`, and `"1se"`. If `"min"`, the tree is pruned using
  the complexity threshold which minimizes the cross-validation error.
  If `"1se"`, the tree is pruned using the largest complexity threshold
  which yields a cross-vaidation error within one standard error of the
  minimum. If `"none"`, the tree is not pruned.

- fit_only:

  Logical. If `TRUE`, only the fitted model is returned. Default is
  `FALSE`.

## Value

If `fit_only = TRUE`, the fitted model is returned. Otherwise, a list
with the following components is returned:

- fit:

  Fitted model. An `rpart` model object.

- tree_info:

  Data frame with tree structure/split information.

- subgroups:

  List of subgroups given by their string representation.

- predictions:

  Student model predictions for the given `X` data.
