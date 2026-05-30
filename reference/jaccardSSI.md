# Jaccard subgroup similarity index

This function computes the Jaccard similarity index between two vectors
of subgroup membership labels, scaling it such that each leaf node
contributes equal weight to the overall similarity.

## Usage

``` r
jaccardSSI(x, y)
```

## Arguments

- x:

  Numeric vector of subgroup memberships. Must be encoded as integers,
  beginning at 0 and be contiguous (i.e., if there are k unique values,
  they must be 0, 1, ..., k-1).

- y:

  Numeric vector of subgroup memberships Must be encoded as integers,
  beginning at 0 and be contiguous (i.e., if there are k unique values,
  they must be 0, 1, ..., k-1).

## Value

Computed Jaccard subgroup similarity metric
