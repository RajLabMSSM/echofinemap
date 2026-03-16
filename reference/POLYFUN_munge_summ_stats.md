# PolyFun: Munge summary stats

Munge summary statistics using the PolyFun implementation of the LDSSC
munge sum stats python script (`munge_polyfun_sumstats.py`). **NOTE:**
This script is kept only for documentation purposes. Please use
[MungeSumstats](https://github.com/neurogenomics/MungeSumstats) instead
as it is far more robust.

## Usage

``` r
POLYFUN_munge_summ_stats(
  fullSS_path,
  polyfun_path = NULL,
  locus_dir = tempdir(),
  sample_size = NULL,
  min_INFO = 0,
  min_MAF = 0.001,
  chi2_cutoff = 30,
  keep_hla = FALSE,
  no_neff = FALSE,
  force_new_munge = FALSE,
  conda_env = "echoR_mini",
  verbose = TRUE
)
```

## Source

` fullSS_path <- echodata::example_fullSS() munged_path <- POLYFUN_munge_summ_stats(fullSS_path=fullSS_path) `

## Arguments

- fullSS_path:

  Path to the full summary statistics file (GWAS or QTL) that you want
  to fine-map. It is usually best to provide the absolute path rather
  than the relative path.

- polyfun_path:

  \[Optional\] Path to PolyFun directory where all the executables and
  reference data are stored. Will be automatically installed if set to
  `NULL` (default).

- locus_dir:

  Locus-specific directory to store results in.

- conda_env:

  Conda environment to use.

- verbose:

  Print messages.

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
[`POLYFUN_prepare_snp_input()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_prepare_snp_input.md),
[`POLYFUN_run_ldsc()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_run_ldsc.md)
