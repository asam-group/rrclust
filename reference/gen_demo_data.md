# Generate random demo data

Generate random demo data and write into a csv file.

## Usage

``` r
gen_demo_data(path = tempdir(), method_name = "kamila", data_size = 1000)
```

## Arguments

- path:

  Path where to store the demo data. Default:
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- method_name:

  Name of the clustering method.

- data_size:

  Size of the simulated data frame. Default: 1000 rows.

## Value

The path of the csv file.

## Author

[Layal Christine Lettry](mailto:layal.lettry@gmail.com)

## Examples

``` r
gen_demo_data()
#> writing:  /tmp/RtmpeeJ8Gx/all/IND_YEARLY_RR.csv 
#> [1] "/tmp/RtmpeeJ8Gx/all/IND_YEARLY_RR.csv"
```
