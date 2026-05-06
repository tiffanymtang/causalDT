# Get decision paths from an rpart model.

Return the decision paths for each leaf node in an `rpart` model as
character strings.

## Usage

``` r
get_rpart_paths(rpart_fit)
```

## Arguments

- rpart_fit:

  An `rpart` object.

## Value

A list of character vectors, where each element corresponds to the
decision path for a leaf node in the `rpart_fit` model.
