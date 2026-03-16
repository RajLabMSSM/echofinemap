# Construct the `FINAMAP` master file

Creates and saves the master file which tells `FINEMAP` where to find
each input file.

## Usage

``` r
FINEMAP_construct_master(
  locus_dir,
  n_samples,
  dir = "FINEMAP",
  dataset_number = 1,
  data.k_path = NULL,
  verbose = TRUE
)
```

## Source

<http://www.christianbenner.com>

` locus_dir <- file.path(tempdir(),echodata::locus_dir) master_path <- echofinemap:::FINEMAP_construct_master(locus_dir=locus_dir, n_samples=25000) `

## See also

Other FINEMAP:
[`FINEMAP()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP.md),
[`FINEMAP_construct_data()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_data.md),
[`FINEMAP_find_executable()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_find_executable.md),
[`FINEMAP_process_results()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_process_results.md)
