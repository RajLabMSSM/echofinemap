# Download annotations for PAINTOR

Download annotations to perform functional fine-mapping with PAINTOR.
Supports three annotation sources:

- annot_paintor:

  The comprehensive PAINTOR functional annotation library (~7GB, 10,000+
  annotations). Downloaded once and cached.

- annot_xgr:

  Annotations from XGR via
  [XGR_query](https://rdrr.io/pkg/echoannot/man/xgr_query.html).

- annot_roadmap:

  Annotations from Roadmap Epigenomics via
  [ROADMAP_query](https://rdrr.io/pkg/echoannot/man/ROADMAP_query.html).

## Usage

``` r
PAINTOR_download_annotations(
  dat_merged,
  locus_dir,
  PT_results_path,
  annot_paintor = NULL,
  annot_sample = FALSE,
  annot_xgr = NULL,
  annot_roadmap = NULL,
  chrom_states = NULL,
  use_annotations = TRUE,
  conda_env = "echoR_mini",
  nThread = 1,
  verbose = TRUE
)
```

## Arguments

- locus_dir:

  Locus-specific directory to store results in.

- annot_paintor:

  Character vector of PAINTOR annotation category keywords to include
  (e.g., `"FANTOM5"`, `"ChromHMM"`, `"DHS"`, `"TFBS"`). Set to `"all"`
  to use all annotations, or `NULL` to skip (default).

- annot_xgr:

  Use annotations from XGR via
  [XGR_query](https://rdrr.io/pkg/echoannot/man/xgr_query.html).

- annot_roadmap:

  Use annotations from Roadmap via
  [ROADMAP_query](https://rdrr.io/pkg/echoannot/man/ROADMAP_query.html).

- chrom_states:

  Filter results by chromatin states.

- use_annotations:

  Whether to perform functional fine-mapping with specified annotations
  (`TRUE`) or simply perform statistical fine-mapping without any
  annotations.

- conda_env:

  Conda environment to use.

- nThread:

  Number of threads to parallelise across (when applicable).

- verbose:

  Print messages.
