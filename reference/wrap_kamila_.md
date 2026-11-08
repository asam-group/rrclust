# Run Kamila algorithm

Run Kamila algorithm.

## Usage

``` r
wrap_kamila_(tl_inp_kamila)
```

## Arguments

- tl_inp_kamila:

  Tidylist of inputs.

## Value

`tl_computation_kamila` with the following data frames:

- `PLOTDATKAM` Data frame containing the clusters factor and the other
  variables.

- `KM_RES_FINAL` Data frame containing the resulting parameters of the
  clustering.

- `CONTVARS` Data frame containing the continuous standardised
  variables.

- `FULL_CONT_DF` Data frame containing the continuous variables used for
  the estimation.

- `FULL_CATEG_DF` Data frame containing the categorical variables used
  for the estimation.

## Author

[Layal Christine Lettry](mailto:layal.lettry@gmail.com)
