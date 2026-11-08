# Separate a string of several words at commas to a vector

Split a character string of several words at commas to a vector.

## Usage

``` r
separate_at_comma(x)
```

## Arguments

- x:

  String of words to split.

## Author

[Christoph Sax](mailto:christoph@cynkra.com)

## Examples

``` r
x <- "sex, nat, resid, benef_type1, benef_type2"
separate_at_comma(x)
#> [1] "sex"         "nat"         "resid"       "benef_type1" "benef_type2"
```
