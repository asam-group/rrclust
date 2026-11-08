# Prepare the data

This wrapper contains all the necessary modules which allow to prepare
the data.

## Usage

``` r
wrap_prepadata_(tl_inp)
```

## Arguments

- tl_inp:

  List of input data frames.

## Value

`tl_mod_catcontvar`: A `tidylist` containing the following data frames:

- `CATEG_DF`: Contains only categorical variables (factors).

- `CONT_DF` : Contains only continuous variables (numeric).

- `CATEG_DF_TS`: Contains only categorical variables (factors), training
  set.

- `CONT_DF_TS` : Contains only continuous variables (numeric), training
  set.

- `CATEG_DF_VS`: Contains only categorical variables (factors),
  validation set.

- `CONT_DF_VS` : Contains only continuous variables (numeric),
  validation set.

## Author

[Layal Christine Lettry](mailto:layal.lettry@gmail.com)
