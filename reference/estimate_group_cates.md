# Subgroup CATE estimation.

This function estimates the conditional average treatment effect for
each subgroup given by the fitted decision tree. The conditional average
treatment effect is estimated as the difference in the average outcome
between treated and control units that fall within each subgroup (i.e.,
each leaf node in the decision tree).

## Usage

``` r
estimate_group_cates(fit, X, Y, Z)
```

## Arguments

- fit:

  Fitted subgroup model used to determine subgroup membership of
  individuals. Typically, this is a `party` or `rpart` object, but any
  model object that can be used to determine subgroup membership via
  `predict(fit, x, type = 'node')` can be used. If
  `predict(fit, x, type = 'node')` returns an error, then subgroups are
  determined based upon the unique values of `predict(fit, x)`.

- X:

  A tibble, data.frame, or matrix of covariates.

- Y:

  A vector of outcomes.

- Z:

  A vector of treatments.

## Value

Estimated subgroup average treatment effects tibble with the following
columns:

- leaf_id:

  Leaf node identifier.

- subgroup:

  String representation of the subgroup.

- estimate:

  Estimated conditional average treatment effect for the subgroup.

- variance:

  Asymptotic variance of the estimated conditional average treatment
  effect.

- .var1:

  Sample variance for treated observations in the subgroup.

- .var0:

  Sample variance for control observations in the subgroup.

- .n1:

  Number of treated observations in the subgroup.

- .n0:

  Number of control observations in the subgroup.

- .sample_idxs:

  Indices of (holdout) observations in the subgroup.

## Examples

``` r
# \donttest{
n <- 50
p <- 3
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
Z <- rbinom(n, 1, 0.5)
Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)

# causal distillation tree output
out <- causalDT(X, Y, Z)
# compute subgroup CATEs manually
group_cates <- estimate_group_cates(
  out$student_fit$fit,
  X = X[out$holdout_idxs, , drop = FALSE],
  Y = Y[out$holdout_idxs],
  Z = Z[out$holdout_idxs]
)
all.equal(out$estimate, group_cates)
#> [1] TRUE
# }
```
