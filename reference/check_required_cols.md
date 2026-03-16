# Check for necessary columns

Check whether the input data (`dat`) has the minimum columns required to
run each fine-mapping method, as well as suggested columns.

## Usage

``` r
check_required_cols(
  dat,
  finemap_methods = NULL,
  case_control = TRUE,
  verbose = TRUE
)
```

## Arguments

- dat:

  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing SNP-level data to fine-map.

- finemap_methods:

  Fine-mapping methods to run. See
  [lfm](https://rajlabmssm.github.io/echofinemap/reference/lfm.md) for a
  list of all fine-mapping methods currently available.

- case_control:

  Whether the summary statistics come from a case-control study (e.g. a
  GWAS of having Alzheimer's Disease or not) (`TRUE`) or a quantitative
  study (e.g. a GWAS of height, or an eQTL) (`FALSE`).

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
finemap_methods <- check_required_cols(dat=dat)
} # }
```
