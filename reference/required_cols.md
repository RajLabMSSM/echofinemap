# Table of required columns

Return a table with the minimum columns required to run each
fine-mapping method, as well as suggested columns.

## Usage

``` r
required_cols(
  case_control = TRUE,
  for_all = c("SNP", "CHR", "POS", "Effect", "StdErr"),
  add_versions = FALSE,
  add_sources = TRUE,
  add_citations = TRUE,
  add_executables = FALSE,
  embed_links = FALSE,
  verbose = TRUE
)
```

## Source

[embedding knitr links](https://stackoverflow.com/a/43670036)

## Arguments

- case_control:

  Whether the summary statistics come from a case-control study (e.g. a
  GWAS of having Alzheimer's Disease or not) (`TRUE`) or a quantitative
  study (e.g. a GWAS of height, or an eQTL) (`FALSE`).

- for_all:

  Columns required for all methods.

- add_versions:

  Add software versions for each method.

- add_sources:

  Add source code URLs for each method.

- add_citations:

  Add citations for each method.

- add_executables:

  Add path to executables for each method.

- embed_links:

  For any columns that contain URLs, embed them as links with shortened
  names.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- required_cols(add_versions=TRUE, add_executables=TRUE)
} # }
```
