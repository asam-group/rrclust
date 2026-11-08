# Estimation of the best number of clusters using the Kamila algorithm

Estimation of best number of clusters using the Kamila algorithm on the
training set.

## Usage

``` r
mod_kstar(PARAM_KAMILA, CATEG_DF_TS, CONT_DF_TS, list = NULL)
```

## Arguments

- PARAM_KAMILA:

  data frame with all needed parameters for the Kamila method, from
  which the following parameters are used:

  - `numberofclusters`: The number of clusters returned by the
    algorithm, i.e. sequence indicating the number of clusters which
    should be investigated to extract the optimal number of clusters.

  - `numinit`: The number of initializations used.

  - `maxiter`: The maximum number of iterations in each run.

  - `calcnumclust`: Character: Method for selecting the number of
    clusters. Setting calcNumClust to ’ps’ uses the prediction strength
    method of Tibshirani &Walther (J. of Comp. and Graphical Stats.
    14(3), 2005).

  - `pred_threshold`: Threshold fixed to 0.8 for well separated
    clusters, i.e. not overlapping.

- CATEG_DF_TS:

  Training set of the pension register containing all categorical
  variables as factors.

- CONT_DF_TS:

  Training set of the pension register containing all the continuous
  variables.

- list:

  List of input data frames.

## Value

a tidylist containing the following tidy data frames:

- `KM_RES` Data frame containing the results of the clustering.

- `PARAM_KAMILA` Data frame with the updated kstar parameter.

## Author

[Layal Christine Lettry](mailto:layal.lettry@gmail.com)
