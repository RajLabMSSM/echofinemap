# Prepare SNP input for PolyFun

PolyFun requires a space-delimited (gzipped or not) file with these
columns:

- `CHR`

- `BP`

- `A1`

- `A2`

## Usage

``` r
POLYFUN_prepare_snp_input(PF.output.path, locus_dir, dat = NULL, nThread = 1)
```

## See also

Other polyfun:
[`POLYFUN()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN.md),
[`POLYFUN_compute_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_compute_priors.md),
[`POLYFUN_download_ref_files()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_download_ref_files.md),
[`POLYFUN_find_folder()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_find_folder.md),
[`POLYFUN_finemapper()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_finemapper.md),
[`POLYFUN_gather_annotations()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_gather_annotations.md),
[`POLYFUN_gather_ldscores()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_gather_ldscores.md),
[`POLYFUN_help()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_help.md),
[`POLYFUN_import_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_import_priors.md),
[`POLYFUN_initialize()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_initialize.md),
[`POLYFUN_munge_summ_stats()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_munge_summ_stats.md),
[`POLYFUN_run_ldsc()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_run_ldsc.md)

## Examples

``` r
if (FALSE) { # \dontrun{
BST1 <- echodata::BST1; locus_dir <- echodata::locus_dir;
dat <- BST1
PF.output.path <- file.path(locus_dir, "PolyFun")
POLYFUN_prepare_snp_input(PF.output.path=PF.output.path, locus_dir=locus_dir, dat=dat)
} # }
```
