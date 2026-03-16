# Prepare priors

Prepare values to be used as per-SNP priors probabilities in
fine-mapping.

## Usage

``` r
prepare_priors(
  dat,
  priors_col = NULL,
  snp_col = "SNP",
  rescale_priors = TRUE,
  verbose = TRUE
)
```

## Arguments

- dat:

  Fine-mapping results data.

- priors_col:

  \[Optional\] Name of the a column in `dat` to extract SNP-wise prior
  probabilities from.

- snp_col:

  Name of the SNP column.

- rescale_priors:

  If prior probabilities are supplied, rescale them from 0-1 (i.e.
  `rescaled_priors = priors / sum(priors)`).

- verbose:

  Print messages.
