
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rrclust <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

The goal of rrclust is to cluster the Swiss Pension Register.

The anonymous data of the Swiss Pension Register (PR) are typically used
to estimate and project (in the short, middle and long term) the
revenues and the expenditures of the Old-Age and Survivors’ Insurance
(OASI). In this perspective, it is essential to have a clear look at the
register’s main statistical features. To better understand it and
benefit more from its richness, we propose analysing the raw data by an
appropriate clustering method.

We face three main difficulties:

1)  As not only continuous but also nominal or categorical variables
    structure the register, we have to choose a clustering method that
    considers any types of variables;

2)  The a priori number of clusters should be in the first step
    determined, and thus the question of how to fix it is essential;

3)  The method should run over big data.

Recently, A. Foss et al. (2016) and A. H. Foss and Markatou (2018)
proposed the kamila Method (KAy-means for MIxed LArge data), which is
specifically designed to manage a clustering process for mixed
distributions. Furthermore, a simple rewriting of the kamila’s algorithm
permits an easy implementation in a map-reduce framework like Hadoop,
thus being run on very large data sets. On the other hand, Tibshirani
and Walther (2005) advocate the use of the “Prediction Strength” as a
measure to find the optimal number of clusters.

We applied the kamila clustering method on the more than 2 000 000
observations of the PR data. The technique allows us to determine the
optimal number of clusters. On this basis, we can analyse the partition
of our data. Indeed, each cluster is then analysed, and its principal
features are described. As a result, it becomes possible to recognise
the similarities and dissimilarities between the OASI pensioners
subgroups according to their socio-demographic characteristics. These
pieces of information are crucial to predicting revenues and
expenditures of the OASI.

## Installation

You can install the development version of rrclust from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("asam-group/rrclust")
```

## Related Paper

One needs to have the proper Pension Register data in order to run this
code. As this register is not public, you can find how it is used and
learn more about the results in this [Working
Paper](https://folia.unifr.ch/unifr/documents/324081).

## Future work

The next step is to implement some classification methods in the package
[`rrml`](https://github.com/asam-group/rrml) which will be applied to
the kamila-clustered Swiss Pension Register thanks to the package
[`rrclust`](https://github.com/asam-group/rrclust).

## Examples

### Example step by step with randomly generated data

``` r
library(rrclust)

# directory for the output
path_out <- tempdir()

# generate random demo data
path_random_data <- gen_demo_data()

# read the demo data
demo_data <- tidylist_read(path_random_data)

# relative path to the params container
path <- file.path(getwd(), "inst", "extdata", "params_kamila_large")

# read PARAM_GLOBAL
tl_PARAM_GLOBAL <- param_tidylist_read(path)

# replace empty variable with the temporary path
tl_PARAM_GLOBAL$PARAM_GLOBAL[["path_data"]] <- stringr::str_remove(dirname(path_random_data), "/all")
PARAM_GLOBAL <- tl_PARAM_GLOBAL$PARAM_GLOBAL |>
  dplyr::mutate_all(as.character) |>
  tidyr::pivot_longer(
    everything(),
    names_to = "key",
    values_to = "value"
  )

# rewrite PARAM_GLOBAL with the demo data path
tidylist_write(tidylist(PARAM_GLOBAL), path = path)

# input
tl_inp_kamila <- mod_inp_kamila(path = path, method_name = "kamila")

# computations
tl_out_kamila <- wrap_kamila(tl_inp_kamila = tl_inp_kamila)

# output
path_out_identifier <- mod_out_kamila(
  path = path,
  path_out = path_out,
  tl_inp_kamila = tl_inp_kamila,
  tl_out_kamila = tl_out_kamila
)

# write the csv files
tidylist_write(
  c(tl_out_kamila, mod_log()),
  path_out_identifier
)

# store parameters (as a reference)
copy_param(path, path_out_identifier)

# access the output
browseURL(path_out_identifier)
```

### Example in one step with randomly generated data

``` r
library(rrclust)

# directory for the output
path_out <- tempdir()

# generate random demo data
path_random_data <- gen_demo_data()

# read the demo data
demo_data <- tidylist_read(path_random_data)

# relative path to the params container
path <- file.path(getwd(), "inst", "extdata", "params_kamila_large")

# read PARAM_GLOBAL
tl_PARAM_GLOBAL <- param_tidylist_read(path)

# replace empty variable with the temporary path
tl_PARAM_GLOBAL$PARAM_GLOBAL[["path_data"]] <- stringr::str_remove(dirname(path_random_data), "/all")
PARAM_GLOBAL <- tl_PARAM_GLOBAL$PARAM_GLOBAL |>
  dplyr::mutate_all(as.character) |>
  tidyr::pivot_longer(
    everything(),
    names_to = "key",
    values_to = "value"
  )

# rewrite PARAM_GLOBAL with the demo data path
tidylist_write(tidylist(PARAM_GLOBAL), path = path)

# execute the whole workflow
rrclust::run_kamila(path = path, path_out = path_out)

# access the output
browseURL(path_out)
```

## Results

You can see the results in the csv named `KM_RES_FINAL.csv`. The best
number of clusters, i.e. the parameter `kstar`, is given by the value of
the variable `num_clust`.
