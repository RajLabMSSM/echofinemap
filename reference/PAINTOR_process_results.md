# PAINTOR: process results

Process PAINTOR fine-mapping results.

## Usage

``` r
PAINTOR_process_results(dat_merged, res_paths, credset_thresh, verbose = TRUE)
```

## Arguments

- dat_merged:

  Merged PAINTOR results.

- res_paths:

  Paths to PAINTOR results.

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

- verbose:

  Print messages.
