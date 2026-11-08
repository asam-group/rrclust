# Splitting the initial dataset into kstar clusters

Splitting the initial dataset into kstar clusters by using the parameter
kstar determined in the module
[`mod_kstar`](https://asam-group.github.io/rrclust/reference/mod_kstar.md).

## Usage

``` r
mod_calc_kamila(
  PARAM_KAMILA,
  CONT_DF,
  CATEG_DF,
  FULL_CONT_DF,
  FULL_CATEG_DF,
  KM_RES,
  list = NULL
)
```

## Arguments

- PARAM_KAMILA:

  data frame with all needed parameters for the Kamila method, from
  which the following parameters are used: - `numinit`: The number of
  initializations used. - `maxiter`: The maximum number of iterations in
  each run. - `param_kstar`: Best number of clusters estimated in the
  module
  [`mod_kstar`](https://asam-group.github.io/rrclust/reference/mod_kstar.md).

- CONT_DF:

  Subset of the pension register containing all the continuous variables
  except for the outcome variables aadr and monthly_pension.

- CATEG_DF:

  Subset of the pension register containing all categorical variables as
  factors except for the nominal variables marital_stat and benef_type.

- FULL_CONT_DF:

  Data frame containing the continuous variables used for the estimation
  plus the outcome variables aadr and monthly_pension.

- FULL_CATEG_DF:

  Data frame containing the categorical variables used for the
  estimation plus the nominal variables marital_stat and benef_type.

- KM_RES:

  Tibble contains the kamila results of the training set.

- list:

  List of input data frames.

## Value

a `tidylist` containing the following tidy data frames:

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
