# Plot Sample Size vs Error

This method generates a plot showing how the required sample size varies
with the maximum allowed relative error.

## Arguments

- x:

  An object of class `SimpleSampleSize` containing sampling options and
  results.

- min_error:

  A numeric value specifying the minimum relative error to consider
  (default is 0.01).

- max_error:

  A numeric value specifying the maximum relative error to consider
  (default is 0.5).

## Value

A `ggplot` object representing the relationship between error and sample
size.
