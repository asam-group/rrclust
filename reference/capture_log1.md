# Capture message, warnings and errors from a R function

Capture message, warnings and errors from a R function

## Usage

``` r
capture_log1(f)
```

## Arguments

- f:

  Function to gather the logs from.

## Value

List of logs.

## References

<https://www.r-bloggers.com/2020/10/capture-message-warnings-and-errors-from-a-r-function/#google_vignette>

## Examples

``` r
#' @examples
#' test_message <- function(a) {
#'   message("this is test from function message.")
#'   return(a)
#' }
#'
#' test_warning <- function(a) {
#'   warning("this is test from function warning.")
#'   return(a)
#' }
#'
#' test_error <- function(a) {
#'   stop("this is test from function error.")
#'   return(a)
#' }
#'
#' capture_log1(test_message)(1)
#'
#' capture_log1(test_warning)(1)
#'
#' capture_log1(test_error)(1)
```
