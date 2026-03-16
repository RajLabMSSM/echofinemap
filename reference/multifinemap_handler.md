# Fine-map using multiple fine-mapping tools

Fine-mapping will be repeated on the same locus using each of the tools
in `finemap_methods`. Then, all results will be merged into the
locus-specific multi-finemap file, along with the original per-SNP
GWAS/QTL summary statistics. Each tools will have the following columns:

- \<tool\>.PP:

  The posterior probability (PP) of a SNP being causal for the trait.
  Though this is a generalization and the exact meaning of PP will
  differ by tools (e.g. Posterior Inclusion Probability for SUSIE).

- \<tool\>.CS:

  Which credible set the SNP is part of (within a locus). If `=0`, then
  the SNP was not part of any credible set. Some tools only produce one
  credible set per locus.

## Usage

``` r
multifinemap_handler(
  dat,
  locus_dir,
  fullSS_path = NULL,
  finemap_methods,
  finemap_args = NULL,
  dataset_type = "GWAS",
  force_new_finemap = FALSE,
  LD_matrix = NULL,
  n_causal = 5,
  compute_n = "ldsc",
  conditioned_snps = NULL,
  credset_thresh = 0.95,
  case_control = TRUE,
  priors_col = NULL,
  verbose = TRUE,
  seed = 2022,
  nThread = 1,
  conda_env = "echoR_mini"
)
```

## Arguments

- dat:

  Fine-mapping results data.

- locus_dir:

  Locus-specific directory to store results in.

- fullSS_path:

  Path to the full summary statistics file (GWAS or QTL) that you want
  to fine-map. It is usually best to provide the absolute path rather
  than the relative path.

- finemap_methods:

  Fine-mapping methods to run. See
  [lfm](https://rajlabmssm.github.io/echofinemap/reference/lfm.md) for a
  list of all fine-mapping methods currently available.

- finemap_args:

  A named nested list containing additional arguments for each
  fine-mapping method. e.g.
  `finemap_args = list(FINEMAP=list(), PAINTOR=list(method=""))`

- dataset_type:

  The kind dataset you're fine-mapping (e.g. GWAS, eQTL, tQTL). This
  will also be used when creating the subdirectory where your results
  will be stored (e.g. *Data/\<dataset_type\>/Kunkle_2019*).

- force_new_finemap:

  By default, if an fine-mapping results file for a given locus is
  already present, then echolocatoR will just use the preexisting file.
  Set `force_new_finemap=T` to override this and re-run fine-mapping.

- LD_matrix:

  Linkage Disequilibrium (LD) matrix to use for fine-mapping.

- n_causal:

  The maximum number of potential causal SNPs per locus. This parameter
  is used somewhat differently by different fine-mapping tools. See
  tool-specific functions for details.

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

- conditioned_snps:

  Which SNPs to conditions on when fine-mapping with (e.g. *COJO*).

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

- case_control:

  Whether the summary statistics come from a case-control study (e.g. a
  GWAS of having Alzheimer's Disease or not) (`TRUE`) or a quantitative
  study (e.g. a GWAS of height, or an eQTL) (`FALSE`).

- priors_col:

  \[Optional\] Name of the a column in `dat` to extract SNP-wise prior
  probabilities from.

- verbose:

  Print messages.

- seed:

  Set the random seed for reproducible results.

- nThread:

  Number of threads to parallelise across (when applicable).

- conda_env:

  Conda environment to use.

## See also

Other finemapping functions:
[`create_method_path()`](https://rajlabmssm.github.io/echofinemap/reference/create_method_path.md),
[`multifinemap()`](https://rajlabmssm.github.io/echofinemap/reference/multifinemap.md),
[`multifinemap_handler_method()`](https://rajlabmssm.github.io/echofinemap/reference/multifinemap_handler_method.md)
