# Fetch PAINTOR functional annotations

Download the comprehensive PAINTOR functional annotation library
(Functional_Annotations.tar.gz, ~7GB) from BOX and cache it locally. The
library contains 10,000+ annotations across functional genomics datasets
(FANTOM5 enhancers, RoadMap ChromHMM, TFBS, DHS, etc.) in hg19
coordinates.

## Usage

``` r
PAINTOR_fetch_annotations(
  cache_dir = tools::R_user_dir("echofinemap", "cache"),
  force_new = FALSE,
  verbose = TRUE
)
```

## Arguments

- cache_dir:

  Directory to cache the downloaded annotations. Defaults to
  `tools::R_user_dir("echofinemap", "cache")`.

- force_new:

  Force re-download even if cached.

- verbose:

  Print messages.

## Value

Path to the extracted annotations directory.
