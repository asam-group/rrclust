# Run the Kamila estimation

This function executes the computations of the Kamila clustering method
according to the parameters which are read in a first step. Eventually,
the outputs are written into csv or excel files.

## Usage

``` r
run_kamila(path, path_out = file.path(tempdir(), "out"), param_replace = NULL)
```

## Arguments

- path:

  Path of the set of parameters set to a particular output.

- path_out:

  Path where the outputs are saved.

- param_replace:

  Parameter which can be used to test some other parameters.
