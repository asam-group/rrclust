# Read files encoded in UTF-8

Read or write files, assuming they are encoded in UTF-8. `read_utf8()`
is roughly `readLines(encoding = 'UTF-8')` (a warning will be issued if
non-UTF8 lines are found), and `write_utf8()` calls
`writeLines(enc2utf8(text), useBytes = TRUE)`.

## Usage

``` r
read_utf8(con, error = FALSE)
```

## Arguments

- con:

  A connection or a file path.

- error:

  Whether to signal an error when non-UTF8 characters are detected (if
  `FALSE`, only a warning message is issued).

## Author

[Christoph Sax](mailto:christoph@cynkra.com)
