# Get split information from an rpart model.

Return the split information for each node in an `rpart` model as a data
frame.

## Usage

``` r
get_rpart_tree_info(rpart_fit, X = NULL, digits = getOption("digits"))
```

## Arguments

- rpart_fit:

  An `rpart` object.

- X:

  Optional data frame containing the features used in the `rpart` model.
  Only used if the model contains categorical variables.

- digits:

  Number of digits to round the split values to.

## Value

A data.frame with information regarding the feature/threshold used for
each split in the `rpart` model.
