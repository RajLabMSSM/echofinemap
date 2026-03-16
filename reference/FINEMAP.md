# Fine-map locus with `FINEMAP`

The stepwise conditional search starts with a causal configuration
containing the SNP with the lowest P-value alone and then iteratively
adds to the causal configuration the SNP given the highest posterior
model probability until no further SNP yields a higher posterior model
probability.\
\
**Output coumns**\
Note that not not all versions of `FINEMAP` will necessarily have all of
these columns, but "PP" and "CS" will always be present regardless of
version.

- PP:

  Per-SNP conditional posterior probability (PP), after conditioning on
  the other SNPs within its respective Credible Set (CS). If a given SNP
  is present in more than one CS, each row will be a list of PP (one per
  CS). These PP were extracted from *.cred* file(s). SNPs that were not
  within the CS are designated `NA`.

- CS:

  The Credible Set (CS) that a given SNP belongs to. If a given SNP is
  present in more than one CS, each row will be a list of CS id numbers
  (one per CS). These CS were extracted from *.cred* file(s).

- PP_snp:

  Per-SNP marginal posterior inclusion probability (PIP), which is the
  probability that a given SNP is in the Credible Set (CS). These PIP
  were extracted from *.snp* file(s). Only SNPs that were excluded from
  the fine-mapping input (e.g. due to not overlapping with the LD panel)
  are designated `NA`.

- PP_config:

  Per-CS posterior probability (PP) that a given Credible Set (CS) (i.e.
  "configuration" of SNPs) is causal. These PP were extracted from
  *.config* file(s). Only SNPs that were not within any CS are
  designated `NA`.

- k:

  Optimized number of causal SNPs, which can be less than or equal to
  the user-supplied `n_causal` argument. These values were extracted
  from *.log* file(s).

## Usage

``` r
FINEMAP(
  dat,
  locus_dir = tempdir(),
  LD_matrix,
  FINEMAP_path = NULL,
  compute_n = "ldsc",
  n_causal = 5,
  model = c("sss", "cond"),
  remove_tmps = FALSE,
  force_new = FALSE,
  credset_thresh = 0.95,
  finemap_version = package_version("1.4.1"),
  priors_col = NULL,
  rescale_priors = TRUE,
  args_list = list(),
  fillNA = 0,
  nThread = 1,
  verbose = TRUE
)
```

## Source

<http://www.christianbenner.com>

## Arguments

- dat:

  Fine-mapping results data.

- locus_dir:

  Locus-specific directory to store results in.

- LD_matrix:

  Linkage Disequilibrium (LD) matrix to use for fine-mapping.

- FINEMAP_path:

  Path to a custom FINEMAP executable to use instead of the ones
  included in echolocatoR. Users can also simply supply "finemap" if
  this command is linked to the executable.

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

- n_causal:

  The maximum number of potential causal SNPs per locus. This parameter
  is used somewhat differently by different fine-mapping tools. See
  tool-specific functions for details.

- model:

  "cond" for stepwise conditional search, "sss" for stochastic shotgun
  search.

- remove_tmps:

  Remove any temporary files generated.

- force_new:

  If saved results already exist in the given `locus_dir`, skip
  re-running FINEMAP and use them (default: `force_new`). Set `TRUE` to
  ignore these files and re-run FINEMAP.

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

- finemap_version:

  Which FINEMAP version to use (specify as a string).

- priors_col:

  \[Optional\] Name of the a column in `dat` to extract SNP-wise prior
  probabilities from.

- rescale_priors:

  If prior probabilities are supplied, rescale them from 0-1 (i.e.
  `rescaled_priors = priors / sum(priors)`).

- args_list:

  A named list of additional arguments to pass to FINEMAP (e.g.:
  args_list = list("–n-iterations"=5000,"–sss"="")). Alternatively, can
  supply a string instead (e.g.: args_list = "–n-iterations 5000 –sss").

- fillNA:

  Fill CS/PP values without fine-mapping results (i.e. `NA`) with some
  default value (e.g. 0).

- nThread:

  Number of threads to parallelise across. Passed to `"--n-threads"` in
  FINEMAP.

- verbose:

  Print messages.

## See also

Other FINEMAP:
[`FINEMAP_construct_data()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_data.md),
[`FINEMAP_construct_master()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_master.md),
[`FINEMAP_find_executable()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_find_executable.md),
[`FINEMAP_process_results()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_process_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
locus_dir <- file.path(tempdir(),echodata::locus_dir)
dat <- echodata::BST1;
LD_matrix <- echofinemap::drop_finemap_cols(echodata::BST1_LD_matrix)
out <- echoLD::subset_common_snps(LD_matrix, dat)
LD_matrix <- out$LD
dat <- out$DT

dat2 <- echofinemap::FINEMAP(dat=dat,
                             locus_dir=locus_dir,
                             LD_matrix=LD_matrix)
} # }
```
