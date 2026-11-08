# Wrapper for the clusters construction

This wrapper contains all the necessary modules which allow to construct
the clusters.

## Usage

``` r
wrap_computation_kamila_(tl_inp, tl_prepadata)
```

## Arguments

- tl_inp:

  List of input data frames of which we use:

  - `PARAM_KAMILA$calc_kstar`: If TRUE, estimates the clusters. Else,
    takes the parameter PARAM_KAMILA\$param_kstar.

  - `PARAM_KAMILA$cont_var_expl`: List of continuous variables chosen as
    explicative variables.

  - `PARAM_KAMILA$categ_var_expl`: List of categorical variables chosen
    as explicative variables.

- tl_prepadata:

  List of data frames prepared in a first step.

## Value

a `tidylist` containing the following tidylists:

- `tl_mod_calc_kamila`: Tidylist containing

  - `PLOTDATKAM` data frame containing the clusters factor and the other
    variables.

  - `KM_RES_FINAL` data frame containing the resulting parameters of the
    clustering.

  - `CONTVARS` data frame containing the continuous standardised
    variables.

  - `FULL_CONT_DF` data frame containing the continuous variables used
    for the estimation.

  - `FULL_CATEG_DF` data frame containing the categorical variables used
    for the estimation.

## Author

[Layal Christine Lettry](mailto:layal.lettry@gmail.com)
