# Run a modified version of S-LDSC

Modifications to S-LDSC include L2-regularization.

## Usage

``` r
POLYFUN_run_ldsc(
  polyfun = NULL,
  output_dir = NULL,
  munged.path,
  min_INFO = 0.6,
  min_MAF = 0.05,
  annotations.path = file.path(polyfun, "example_data/annotations."),
  weights.path = file.path(polyfun, "example_data/weights."),
  prefix = "LDSC",
  chrom = "all",
  compute_ldscores = FALSE,
  allow_missing_SNPs = TRUE,
  munged_path =
    "/sc/arion/projects/pd-omics/tools/polyfun/Nalls23andMe_2019.sumstats_munged.parquet",
  ref.prefix = "/sc/arion/projects/pd-omics/data/1000_Genomes/Phase1/1000G.mac5eur.",
  freq.prefix = "/sc/arion/projects/pd-omics/tools/polyfun/1000G_frq/1000G.mac5eur.",
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
[`POLYFUN_import_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_import_priors.md),
[`POLYFUN_initialize()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_initialize.md),
[`POLYFUN_munge_summ_stats()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_munge_summ_stats.md),
[`POLYFUN_prepare_snp_input()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_prepare_snp_input.md)
