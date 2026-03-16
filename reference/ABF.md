# Fine-map with ABF

Conduct statistical fine-mapping with Approximate Bayes Factor (ABF) via
[finemap.abf](https://rdrr.io/pkg/coloc/man/finemap.abf.html).

## Usage

``` r
ABF(
  dat,
  credset_thresh = 0.95,
  compute_n = "ldsc",
  sdY = NULL,
  case_control = TRUE,
  verbose = TRUE
)
```

## Source

[JB Maller et al., Bayesian refinement of association signals for 14
loci in 3 common diseases. Nature Genetics. 44, 1294–1301
(2012).](https://www.nature.com/articles/ng.2435)

[J Wakefield, A bayesian measure of the probability of false discovery
in genetic epidemiology studies. American Journal of Human Genetics. 81,
208–227 (2007).](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1950810/)

## Arguments

- dat:

  Fine-mapping results data.

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

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

- sdY:

  Standard deviation of quantitative trait.

- case_control:

  Whether the summary statistics come from a case-control study (e.g. a
  GWAS of having Alzheimer's Disease or not) (`TRUE`) or a quantitative
  study (e.g. a GWAS of height, or an eQTL) (`FALSE`).

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- echodata::LRRK2
dat2 <- echofinemap::ABF(dat=dat) 
} # }
```
