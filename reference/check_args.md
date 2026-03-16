# Check arguments

Check a list of arguments for a given function and:

- Remove any that do not match available arguments for that method.

- Set default values for any arguments that are NULL or not provided.

## Usage

``` r
check_args(finemap_args, finemap_method, max_values = 1, verbose = TRUE)
```

## Source

` finemap_args <- list(SUSIE=list("typo1"=1,rescale_priors=TRUE), FINEMAP=list("FINEMAP_path"=NULL,"model"=NULL)) fma <- check_args(finemap_args=finemap_args, finemap_method="SUSIE") `

## Arguments

- finemap_args:

  A named nested list containing additional arguments for each
  fine-mapping method. e.g.
  `finemap_args = list(FINEMAP=list(), PAINTOR=list(method=""))`

- max_values:

  Max number of values to return for any argument.

- verbose:

  Print messages.

## Value

Named list of arguments for a specific `finemap_method`.
