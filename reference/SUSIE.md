# Fine-map with SUSIE

Sum of Single Effects (SuSiE): Iterative Bayesian Step-wise Selection.

## Usage

``` r
SUSIE(
  dat,
  LD_matrix,
  case_control = TRUE,
  max_causal = 5,
  compute_n = "ldsc",
  priors_col = NULL,
  rescale_priors = TRUE,
  credset_thresh = 0.95,
  scaled_prior_variance = 0.001,
  estimate_residual_variance = FALSE,
  estimate_prior_variance = TRUE,
  residual_variance = NULL,
  max_iter = 100,
  estimate_prior_method = "optim",
  var_y = NULL,
  plot_track_fit = FALSE,
  return_all_CS = TRUE,
  file_prefix = file.path(tempdir(), "SUSIE"),
  verbose = TRUE
)
```

## Source

[GitHub](https://stephenslab.github.io/susieR/)
[Publication](https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssb.12388)

## Arguments

- dat:

  Fine-mapping results data.

- LD_matrix:

  Linkage Disequilibrium (LD) matrix to use for fine-mapping.

- case_control:

  Whether the summary statistics come from a case-control study (e.g. a
  GWAS of having Alzheimer's Disease or not) (`TRUE`) or a quantitative
  study (e.g. a GWAS of height, or an eQTL) (`FALSE`).

- max_causal:

  The maximum number of non-zero effects (and thus causal variants).

- compute_n:

  How to compute per-SNP sample size (new column "N").\
  If the column "N" is already present in `dat`, this column will be
  used to extract per-SNP sample sizes and the argument `compute_n` will
  be ignored.\
  If the column "N" is *not* present in `dat`, one of the following
  options can be supplied to `compute_n`:

  `0`

  :   N will not be computed.

  `>0`

  :   If any number \>0 is provided, that value will be set as N for
      every row. \*\*Note\*\*: Computing N this way is incorrect and
      should be avoided if at all possible.

  `"sum"`

  :   N will be computed as: cases (N_CAS) + controls (N_CON), so long
      as both columns are present.

  `"ldsc"`

  :   N will be computed as effective sample size: Neff
      =(N_CAS+N_CON)\*(N_CAS/(N_CAS+N_CON)) /
      mean((N_CAS/(N_CAS+N_CON))(N_CAS+N_CON)==max(N_CAS+N_CON)).

  `"giant"`

  :   N will be computed as effective sample size: Neff = 2 / (1/N_CAS +
      1/N_CON).

  `"metal"`

  :   N will be computed as effective sample size: Neff = 4 / (1/N_CAS +
      1/N_CON).

- priors_col:

  \[Optional\] Name of the a column in `dat` to extract SNP-wise prior
  probabilities from.

- rescale_priors:

  If prior probabilities are supplied, rescale them from 0-1 (i.e.
  `rescaled_priors = priors / sum(priors)`).

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

- scaled_prior_variance:

  The prior variance, divided by `var(y)` (or by `(1/(n-1))yty` for
  `susie_suff_stat`); that is, the prior variance of each non-zero
  element of b is `var(y) * scaled_prior_variance`. The value provided
  should be either a scalar or a vector of length `L`. If
  `estimate_prior_variance = TRUE`, this provides initial estimates of
  the prior variances.

- estimate_residual_variance:

  If `estimate_residual_variance = TRUE`, the residual variance is
  estimated, using `residual_variance` as an initial value. If
  `estimate_residual_variance = FALSE`, the residual variance is fixed
  to the value supplied by `residual_variance`.

- estimate_prior_variance:

  If `estimate_prior_variance = TRUE`, the prior variance is estimated
  (this is a separate parameter for each of the L effects). If provided,
  `scaled_prior_variance` is then used as an initial value for the
  optimization. When `estimate_prior_variance = FALSE`, the prior
  variance for each of the L effects is determined by the value supplied
  to `scaled_prior_variance`.

- residual_variance:

  Variance of the residual. If `estimate_residual_variance = TRUE`, this
  value provides the initial estimate of the residual variance. By
  default, it is set to `var(y)` in `susie` and `(1/(n-1))yty` in
  `susie_suff_stat`.

- max_iter:

  Maximum number of IBSS iterations to perform.

- estimate_prior_method:

  The method used for estimating prior variance. When
  `estimate_prior_method = "simple"` is used, the likelihood at the
  specified prior variance is compared to the likelihood at a variance
  of zero, and the setting with the larger likelihood is retained.

- var_y:

  \[Optional\] User-supplied phenotypic variance value(s). Can be one of
  the following:

  `NULL`:

  :   Variance will be inferred automatically by SUSIE.

  Numeric vector:

  :   Variance will be computed directly from vector.

  Character string:

  :   The name of a column in `dat` to extract a numeric vector from to
      compute variance.

  "case_control"

  :   Variance will be inferred from the proportion of cases/controls in
      the study. Only works when both "N_cases" and "N_controls" are
      columns in `dat`.

- plot_track_fit:

  Record each iteration and make a GIF of the fine-mapping algorithm
  learning the causal variants. **WARNING!:** Making this plot can take
  a long time if there's many iterations.

- return_all_CS:

  If \>1 Credible Set is identified, return them all (`TRUE`), or just
  the first (`FALSE`).

- file_prefix:

  Prefix to path of output plot file. If not specified, the plot, or
  plots, will be saved to a temporary directory generated using
  [`tempdir`](https://rdrr.io/r/base/tempfile.html).

- verbose:

  Print messages.

## Details

**Notes on convergence:** susieR will often give the warning:
`IBSS algorithm did not converge in 100 iterations!`. This means the
results might not necessarily be reliable. There's several things you
can try to avoid this:

- Make sure `susieR` is up-to-date:
  `devtools::install_github("stephenslab/susieR@0.9.0")`

- Increase `max_causal` (e.g. 5 =\> 10).

- Increase `max_iter` (e.g. 100 =\> 1000), though this will take longer.

- Decrease the locus window size, which will also speed up the algorithm
  but potentially miss causal variants far from the lead SNP.

Changing `estimate_prior_method` does not seem to affect convergence
warnings.

**Notes on variance:** [GitHub
Issue](https://github.com/stephenslab/susieR/issues/90) If
`estimate_residual_variance=TRUE` *without* providing `var_y` *and*
`L>1`, susieR will throw error:
`Estimating residual variance failed: the estimated value is negative`
Running susieR with `var_y = var(b)` provides *exactly* the same
results.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
LD_matrix <- echodata::BST1_LD_matrix
dat2 <- echofinemap::SUSIE(dat=dat, LD_matrix=LD_matrix)
} # }
```
