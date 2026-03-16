# Run susieR fine-mapping

Finds the proper function to call from susieR (which can depend on which
version is installed) and conducts statistical or functional
fine-mapping using GWAS/QTL summary stats.

## Usage

``` r
SUSIE_run(
  dat,
  LD_matrix,
  sample_size,
  max_causal,
  scaled_prior_variance,
  estimate_prior_variance,
  residual_variance,
  max_iter,
  estimate_prior_method,
  estimate_residual_variance,
  prior_weights,
  credset_thresh,
  plot_track_fit,
  verbose = TRUE
)
```
