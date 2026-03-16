# Run *PolyFun+SUSIE* fine-mapping pipeline

Run *PolyFun+SUSIE* fine-mapping pipeline

## Usage

``` r
POLYFUN_finemapper(
  polyfun = NULL,
  dat = NULL,
  npz_gz_LD = NULL,
  locus = NULL,
  sample_size = NULL,
  locus_dir,
  n_causal = 5,
  method = "susie",
  h2_path = NULL,
  conda_env = "echoR_mini",
  verbose = TRUE
)
```

## Source

https://www.nature.com/articles/s41588-020-00735-5

## See also

Other polyfun:
[`POLYFUN()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN.md),
[`POLYFUN_compute_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_compute_priors.md),
[`POLYFUN_download_ref_files()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_download_ref_files.md),
[`POLYFUN_find_folder()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_find_folder.md),
[`POLYFUN_gather_annotations()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_gather_annotations.md),
[`POLYFUN_gather_ldscores()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_gather_ldscores.md),
[`POLYFUN_help()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_help.md),
[`POLYFUN_import_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_import_priors.md),
[`POLYFUN_initialize()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_initialize.md),
[`POLYFUN_munge_summ_stats()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_munge_summ_stats.md),
[`POLYFUN_prepare_snp_input()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_prepare_snp_input.md),
[`POLYFUN_run_ldsc()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_run_ldsc.md)
