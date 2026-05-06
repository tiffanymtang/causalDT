# Get decision paths from a party model.

Return the decision paths for each leaf node in a `party` model as
character strings.

## Usage

``` r
get_party_paths(party_fit)
```

## Arguments

- party_fit:

  A `party` object.

## Value

A list of character vectors, where each element corresponds to the
decision path for a leaf node in the `party_fit` model.
