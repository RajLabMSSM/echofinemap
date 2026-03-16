# Drop fine-mapping columns

Drop all method-specific and/or multi-tool summary columns from data.
This is helpful when ensuring duplicate columns aren't created when
re-running fine-mapping with the same method.

## Usage

``` r
drop_finemap_cols(
  dat,
  finemap_methods = NULL,
  summary_cols = c("Support", "Consensus_SNP", "mean\\.PP"),
  verbose = TRUE
)
```

## Arguments

- dat:

  Fine-mapping results data.

- finemap_methods:

  Fine-mapping methods to run. See
  [lfm](https://rajlabmssm.github.io/echofinemap/reference/lfm.md) for a
  list of all fine-mapping methods currently available.

- summary_cols:

  Multi-tool summary columns to drop.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dat2 <- echofinemap::drop_finemap_cols(dat=dat)
} # }
```
