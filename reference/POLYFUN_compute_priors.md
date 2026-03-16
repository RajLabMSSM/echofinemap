# PolyFun: recompute priors

Recompute SNP-wise priors from summary stats. **Important not on
duplicate SNPs:** Make sure you have removed all duplicate SNPs from
your full summary stats file (`fullSS_path`) before running this
function. Or simply use the `remove_dup=TRUE` argument. **Important note
on file formats:** Make sure your full summary stats file
(`fullSS_path`) is tab-delimited (NOT space-delimited). This will be
done automatically if you set `remove_dup=TRUE`.

## Usage

``` r
POLYFUN_compute_priors(
  fullSS_path,
  remove_dup = TRUE,
  polyfun_path = NULL,
  locus_dir = tempdir(),
  sample_size = NULL,
  min_INFO = 0,
  min_MAF = 0.001,
  skip_ckmedian = FALSE,
  num_bins = NULL,
  annotations_path = NULL,
  weights_path = NULL,
  prefix = "dataset1",
  chrom = "all",
  compute_ldscores = FALSE,
  allow_missing_SNPs = TRUE,
  ref_prefix = NULL,
  remove_tmps = TRUE,
  conda_env = "echoR_mini",
  verbose = TRUE
)
```

## Arguments

- fullSS_path:

  Path to the full summary statistics file (GWAS or QTL) that you want
  to fine-map. It is usually best to provide the absolute path rather
  than the relative path.

- remove_dup:

  Whether to remove duplicate SNPs from the full summary stats file
  before running PolyFun.

- polyfun_path:

  \[Optional\] Path to PolyFun directory where all the executables and
  reference data are stored. Will be automatically installed if set to
  `NULL` (default).

- locus_dir:

  Locus-specific directory to store results in.

- sample_size:

  Dataset sample size.

- min_INFO:

  Minimum per-SNP INFO criterion score.

- min_MAF:

  Minimum per-SNP MAF.

- skip_ckmedian:

  SKip ckmedian step.

- num_bins:

  Number of bins to use for the non-parametric functional enrichment
  approach. 30 is a reasonable value. If `NULL`, the default PolyFun
  behavior is used.

- annotations_path:

  Path prefix to annotation files \#' (e.g.
  "path/to/files/annotations."). If `NULL`, will simply use example
  files included with PolyFun.

- weights_path:

  Path prefix to weights files (e.g. "path/to/files/weights."). If
  `NULL`, will simply use example files included with PolyFun.

- prefix:

  Dataset prefix name.

- chrom:

  Which chromosome to query.

- compute_ldscores:

  Whether to compute per-SNP heritability with LDSCore regression.

- allow_missing_SNPs:

  Whether or not to allow missing SNPs.

- ref_prefix:

  Prefix of path leading to reference files.

- remove_tmps:

  Remove temporary files.

- conda_env:

  Conda environment to use.

- verbose:

  Print messages.

## See also

Other polyfun:
[`POLYFUN()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN.md),
[`POLYFUN_download_ref_files()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_download_ref_files.md),
[`POLYFUN_find_folder()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_find_folder.md),
[`POLYFUN_finemapper()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_finemapper.md),
[`POLYFUN_gather_annotations()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_gather_annotations.md),
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
fullSS_path <- echodata::example_fullSS()
ldsc_files <- echofinemap:::POLYFUN_compute_priors(fullSS_path=fullSS_path)
} # }
```
