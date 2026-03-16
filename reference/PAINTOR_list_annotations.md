# List PAINTOR annotation BED files

List all BED annotation files from the PAINTOR functional annotation
library, optionally filtered by category keyword.

## Usage

``` r
PAINTOR_list_annotations(annot_dir, categories = NULL, verbose = TRUE)
```

## Arguments

- annot_dir:

  Path to the extracted annotations directory (from
  `PAINTOR_fetch_annotations`).

- categories:

  Character vector of category keywords to filter by (e.g., `"FANTOM5"`,
  `"ChromHMM"`, `"DHS"`, `"TFBS"`). If `NULL`, returns all BED files.

- verbose:

  Print messages.

## Value

Character vector of BED file paths.
