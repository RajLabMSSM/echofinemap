# Compute phenotype variance

Compute phenotype variance (`var_y` in susieR).

## Usage

``` r
get_pheno_variance(dat, case_control, var_y, verbose = TRUE)
```

## Arguments

- dat:

  Fine-mapping results data.

- case_control:

  Whether the summary statistics come from a case-control study (e.g. a
  GWAS of having Alzheimer's Disease or not) (`TRUE`) or a quantitative
  study (e.g. a GWAS of height, or an eQTL) (`FALSE`).

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

- verbose:

  Print messages.
