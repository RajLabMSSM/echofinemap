# Prepare input files for `FINEMAP`

Creates and saves 1) the summary stats file, and 2) the LD matrix.
"Columns beta and se are required for fine-mapping. Column maf is needed
to output posterior effect size estimates on the allelic scale. All
other columns are not required for computations and can be specified
arbitrarily."

## Usage

``` r
FINEMAP_construct_data(dat, locus_dir, LD_matrix, nThread = 1, verbose = TRUE)
```

## Source

<http://www.christianbenner.com>

` locus_dir <- echodata::locus_dir; dat <- echodata::BST1; LD_matrix <- echodata::BST1_LD_matrix dir.create(file.path(locus_dir,"FINEMAP"), showWarnings = FALSE, recursive = TRUE) out <- echoLD::subset_common_snps(LD_matrix=LD_matrix, dat=dat) LD_matrix <- out$LD dat <- out$DT dat_paths <- echofinemap:::FINEMAP_construct_data(dat=dat, locus_dir=locus_dir, LD_matrix=LD_matrix) `

## See also

Other FINEMAP:
[`FINEMAP()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP.md),
[`FINEMAP_construct_master()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_master.md),
[`FINEMAP_find_executable()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_find_executable.md),
[`FINEMAP_process_results()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_process_results.md)
