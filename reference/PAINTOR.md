# PAINTOR: fine-map

Run the full PAINTOR fine-mapping pipeline.

## Usage

``` r
PAINTOR(
  dat,
  LD_matrix,
  locus_dir,
  annot = NULL,
  zscore_col = "ZSCORE",
  tstat_col = "tstat",
  max_causal = 1,
  use_annotations = FALSE,
  annot_paintor = NULL,
  annot_xgr = NULL,
  annot_roadmap = NULL,
  chrom_states = NULL,
  credset_thresh = 0.95,
  superpopulation = "EUR",
  LD_reference = "1KGphase3",
  force_new_LD = FALSE,
  method = c("mcmc", "enumerate"),
  seed = 2022,
  paintor_path = NULL,
  force_reinstall = FALSE,
  conda_env = "echoR_mini",
  nThread = 1,
  verbose = TRUE
)
```

## Source

[GitHub](https://github.com/gkichaev/PAINTOR_V3.0) [LD
Tutorial](https://github.com/gkichaev/PAINTOR_V3.0/wiki/2a.-Computing-1000-genomes-LD)
[Input file
formats](https://github.com/gkichaev/PAINTOR_V3.0/wiki/2.-Input-Files-and-Formats)

## Arguments

- dat:

  Fine-mapping results data.

- LD_matrix:

  Linkage Disequilibrium (LD) matrix to use for fine-mapping.

- locus_dir:

  Locus-specific directory to store results in.

- annot:

  Custom annotations to use for functional fine-mapping.

- zscore_col:

  Name of the column containing the normalized Z-statistic.

- tstat_col:

  \[Optional\] Name of the column containing the t-statistic.

- max_causal:

  Maximum number of causal SNPs.

- use_annotations:

  Whether to perform functional fine-mapping with specified annotations
  (`TRUE`) or simply perform statistical fine-mapping without any
  annotations.

- annot_paintor:

  Character vector of PAINTOR annotation category keywords (e.g.,
  `"FANTOM5"`, `"ChromHMM"`, `"DHS"`, `"TFBS"`). Set to `"all"` to use
  all 10,000+ annotations. Downloads the ~7GB annotation library on
  first use and caches it.

- annot_xgr:

  Use annotations from XGR via
  [XGR_query](https://rdrr.io/pkg/echoannot/man/xgr_query.html).

- annot_roadmap:

  Use annotations from Roadmap via
  [ROADMAP_query](https://rdrr.io/pkg/echoannot/man/ROADMAP_query.html).

- chrom_states:

  Filter results by chromatin states.

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

- superpopulation:

  The ancestry of each population in `dat`. Can be one (applied to all
  dataset) or more (one per dataset). When users don't supply their own
  Linkage Disequilibrium (LD) matrix/matrices via the `LD_matrix`
  argument, this information is used to gather LD matrices from the
  reference panel(s) using
  [get_LD](https://rdrr.io/pkg/echoLD/man/get_LD.html).

- LD_reference:

  Name of the LD reference panel.

- force_new_LD:

  Force new LD subset.

- method:

  `"enumerate"` is actually faster when `max_causal` is small (\<3), but
  far larger `max_causal` use `"mcmc"`.

- seed:

  Set the random seed for reproducible results.

- paintor_path:

  \[Optional\] Path to PAINTOR executable. Will be automatically
  installed if set to `NULL` (default).

- force_reinstall:

  Force reinstallation of PAINTOR.

- conda_env:

  Conda environment to use.

- nThread:

  Number of threads to parallelise across (when applicable).

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::BST1
## For example only;
## normally you need to compute ZSCORE using the
## full genome-wide summary stats.
dat[,ZSCORE:=(-log10(P))]
LD_matrix <- echodata::BST1_LD_matrix
locus_dir <- file.path(tempdir(),echodata::locus_dir)
dat2 <- PAINTOR(dat = dat,
                locus_dir = locus_dir,
                LD_matrix = LD_matrix,
                max_causal = 2,
                method = "enumerate")
} # }
```
