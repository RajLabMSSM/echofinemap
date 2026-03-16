# Import pre-computed priors

Import SNP-wise prior probabilities pre-computed from many UK Biobank
traits. This function handles finding the intersection of SNPS that
exist in the input GWAS summary stats `dat` and the pre-computed priors
that come shipped with PolyFun. Then, it saves this subset as a new file
for PolyFun (or other fine-mapping tools) to use as input. Uses the
`extract_snpvar.py` script from PolyFun.

## Usage

``` r
POLYFUN_import_priors(
  locus_dir,
  dat = NULL,
  polyfun = NULL,
  force_new_priors = TRUE,
  remove_tmps = FALSE,
  nThread = 1,
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
[`POLYFUN_finemapper()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_finemapper.md),
[`POLYFUN_gather_annotations()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_gather_annotations.md),
[`POLYFUN_gather_ldscores()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_gather_ldscores.md),
[`POLYFUN_help()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_help.md),
[`POLYFUN_initialize()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_initialize.md),
[`POLYFUN_munge_summ_stats()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_munge_summ_stats.md),
[`POLYFUN_prepare_snp_input()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_prepare_snp_input.md),
[`POLYFUN_run_ldsc()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_run_ldsc.md)
