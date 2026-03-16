# Find and import PolyFun annotation files

Find and import PolyFun annotation files

## Usage

``` r
POLYFUN_gather_annotations(
  chromosomes = seq_len(22),
  subset_snps = NULL,
  polyfun_annots
)
```

## See also

Other polyfun:
[`POLYFUN()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN.md),
[`POLYFUN_compute_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_compute_priors.md),
[`POLYFUN_download_ref_files()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_download_ref_files.md),
[`POLYFUN_find_folder()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_find_folder.md),
[`POLYFUN_finemapper()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_finemapper.md),
[`POLYFUN_gather_ldscores()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_gather_ldscores.md),
[`POLYFUN_help()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_help.md),
[`POLYFUN_import_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_import_priors.md),
[`POLYFUN_initialize()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_initialize.md),
[`POLYFUN_munge_summ_stats()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_munge_summ_stats.md),
[`POLYFUN_prepare_snp_input()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_prepare_snp_input.md),
[`POLYFUN_run_ldsc()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_run_ldsc.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
annot_DT <- POLYFUN_gather_annotations(
    chromosomes = dat$CHR[1],
    subset_snps = subset_snps,
    polyfun_annots = file.path(
        "/pd-omics/tools/polyfun",
        "annotations/baselineLF2.2.UKB"
    )
)
} # }
```
