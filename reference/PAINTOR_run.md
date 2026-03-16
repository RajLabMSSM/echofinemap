# Run PAINTOR

Run PAINTOR executable.

## Usage

``` r
PAINTOR_run(
  paintor_path = NULL,
  PT_results_path,
  inputFile_path,
  ld_paths,
  zscore_cols,
  max_causal,
  Gname = "Enrichment.Estimates.txt",
  RESname = "results.txt",
  ANname = "annotations.txt",
  annotation_names = NULL,
  method = c("mcmc", "enumerate"),
  seed = 2022,
  auto_restart = FALSE,
  verbose = TRUE
)
```

## Arguments

- paintor_path:

  \[Optional\] Path to PAINTOR executable. Will be automatically
  installed if set to `NULL` (default).

- max_causal:

  Maximum number of causal SNPs.

- method:

  `"enumerate"` is actually faster when `max_causal` is small (\<3), but
  far larger `max_causal` use `"mcmc"`.

- seed:

  Set the random seed for reproducible results.

- auto_restart:

  Automatically rerun PAINTOR if it fails the first time.

- verbose:

  Print messages.
