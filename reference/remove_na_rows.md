# Remove NA rows

Remove rows with NAs in one or more specific columns. If the column is
not present, it will not be used for filtering.

## Usage

``` r
remove_na_rows(
  dat,
  cols = c("Effect", "P", "StdErr", "SNP", "MAF"),
  verbose = TRUE
)
```

## Arguments

- dat:

  Data

- cols:

  Column names.

- verbose:

  Print messages.

## Value

Filtered data
