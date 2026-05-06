# Plot causal distillation tree object

Visualize the subgroups (i.e., the student tree) from a causal
distillation tree object.

## Usage

``` r
plot_cdt(cdt, show_digits = 2)
```

## Arguments

- cdt:

  A causal distillation tree object, typically the output of
  [`causalDT`](https://tiffanymtang.github.io/causalDT/reference/causalDT.md).

- show_digits:

  Number of digits to show in the plot labels. Default is 2.

## Value

A plot of the causal distillation tree.

## Examples

``` r
# \donttest{
n <- 200
p <- 10
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
Z <- rbinom(n, 1, 0.5)
Y <- 2 * Z * (X[, 1] > 0) + X[, 2] + rnorm(n, 0.1)

cdt <- causalDT(X, Y, Z)
plot_cdt(cdt)
#> Warning: Ignoring unknown parameters: `label.size`

# }
```
