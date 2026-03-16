# FINEMAP: process results

Post-processing of `FINEMAP` results.

## Usage

``` r
FINEMAP_process_results(
  dat,
  locus_dir,
  credset_thresh = 0.95,
  pvalue_thresh = 0.05,
  finemap_version = package_version("1.4.1"),
  file_options = NULL,
  sort_by_CS = TRUE,
  verbose = TRUE
)
```

## Source

<http://www.christianbenner.com>

` locus_dir <- file.path(tempdir(), echodata::locus_dir) dat <- echodata::BST1; dat2 <- echofinemap:::FINEMAP_process_results(dat=dat, locus_dir=locus_dir) `

## See also

Other FINEMAP:
[`FINEMAP()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP.md),
[`FINEMAP_construct_data()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_data.md),
[`FINEMAP_construct_master()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_master.md),
[`FINEMAP_find_executable()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_find_executable.md)
