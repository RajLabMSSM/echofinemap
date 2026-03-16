# Run *GCTA-COJO*

Main function to run either the conditional stepwise procedure
(genome-wide) or the conditional analysis (locus-specific) from
*GCTA-COJO*.

## Usage

``` r
COJO(
  dat,
  locus_dir,
  bfile = file.path(locus_dir, "LD/plink"),
  fullSS_path = NULL,
  conditioned_snps = NULL,
  exclude = NULL,
  prefix = "cojo",
  run_stepwise = TRUE,
  run_conditional = FALSE,
  run_joint = FALSE,
  credset_thresh = 0.95,
  freq_cutoff = 0.1,
  compute_n = "ldsc",
  colmap = echodata::construct_colmap(),
  full_genome = FALSE,
  gcta_path = echoconda::find_executables_remote(tool = "gcta")[[1]],
  verbose = TRUE,
  ...
)
```

## Source

[COJO
documentation](https://yanglab.westlake.edu.cn/software/gcta/#COJO)
Publication 1
([doi:10.1016/j.ajhg.2010.11.011](https://doi.org/10.1016/j.ajhg.2010.11.011)
) Publication 2 ([doi:10.1038/ng.2213](https://doi.org/10.1038/ng.2213)
)

## Arguments

- dat:

  Fine-mapping results data.

- locus_dir:

  Locus-specific directory to store results in.

- bfile:

  Input PLINK binary PED files, e.g. test.fam, test.bim and test.bed
  (see PLINK user manual for details).

- fullSS_path:

  Path to the full summary statistics file (GWAS or QTL) that you want
  to fine-map. It is usually best to provide the absolute path rather
  than the relative path.

- conditioned_snps:

  Which SNPs to conditions on when fine-mapping with (e.g. *COJO*).

- exclude:

  Specify a list of SNPs to be excluded from the analysis.

- prefix:

  Prefix to use for file names.

- run_stepwise:

  `--cojo-slct`: Perform a stepwise model selection procedure to select
  independently associated SNPs. Results will be saved in a *\*.jma*
  file with additional file *\*.jma.ldr* showing the LD correlations
  between the SNPs.

- run_conditional:

  `--cojo-cond`: Perform association analysis of the included SNPs
  conditional on the given list of SNPs. Results will be saved in a
  *\*.cma*. The conditional SNP effects (i.e. bC) will be labelled as
  "NA" if the multivariate correlation between the SNP in question and
  all the covariate SNPs is \> 0.9.

- run_joint:

  `--cojo-joint`: Fit all the included SNPs to estimate their joint
  effects without model selection. Results will be saved in a *\*.jma*
  file with additional file *\*.jma.ldr*.

- credset_thresh:

  The minimum mean Posterior Probability (across all fine-mapping
  methods used) of SNPs to be included in the "mean.CS" column.

- freq_cutoff:

  Minimum variant frequency cutoff.

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

- colmap:

  Column mappings object. Uses
  [construct_colmap](https://rdrr.io/pkg/echodata/man/construct_colmap.html)
  by default.

- full_genome:

  Whether to run GCTA-COJO across genome-wide (`TRUE`), or within a
  specific locus (default: `FALSE`)

- gcta_path:

  Path to the GCTA-COJO executable.

- verbose:

  Print messages.

- ...:

  Arguments passed on to
  [`COJO_args`](https://rajlabmssm.github.io/echofinemap/reference/COJO_args.md)

  `cojo_file`

  :   Input the summary-level statistics from a meta-analysis GWAS (or a
      single GWAS).

  `out`

  :   Specify output root filename.

  `cojo_slct`

  :   Perform a stepwise model selection procedure to select
      independently associated SNPs. Results will be saved in a \*.jma
      file with additional file \*.jma.ldr showing the LD correlations
      between the SNPs.

  `cojo_cond`

  :   Perform association analysis of the included SNPs conditional on
      the given list of SNPs. Results will be saved in a \*.cma. The
      conditional SNP effects (i.e. bC) will be labelled as "NA" if the
      multivariate correlation between the SNP in question and all the
      covariate SNPs is \> 0.9.

  `cojo_joint`

  :   Fit all the included SNPs to estimate their joint effects without
      model selection. Results will be saved in a \*.jma file with
      additional file \*.jma.ldr showing the LD correlations between the
      SNPs.

  `maf`

  :   Exclude SNPs with minor allele frequency (MAF) less than a
      specified value, e.g. 0.01.

  `max_maf`

  :   Include SNPs with MAF less than a specified value, e.g. 0.1.

  `cojo_top_SNPs`

  :   Perform a stepwise model selection procedure to select a fixed
      number of independently associated SNPs without a p-value
      threshold. The output format is the same as that from
      `--cojo-slct`.

  `cojo_p`

  :   Threshold p-value to declare a genome-wide significant hit. The
      default value is 5e-8 if not specified. This option is only valid
      in conjunction with the option `--cojo-slct`. Note: it will be
      extremely time-consuming if you set a very low significance level,
      e.g. 5e-3.

  `cojo_wind`

  :   Specify a distance d (in Kb unit). It is assumed that SNPs more
      than d Kb away from each other are in complete linkage
      equilibrium. The default value is 10000 Kb (i.e. 10 Mb) if not
      specified.

  `cojo_collinear`

  :   During the model selection procedure, the program will check the
      collinearity between the SNPs that have already been selected and
      a SNP to be tested. The testing SNP will not be selected if its
      multiple regression R2 on the selected SNPs is greater than the
      cutoff value. By default, the cutoff value is 0.9 if not
      specified.

  `diff_freq`

  :   To check the difference in allele frequency of each SNP between
      the GWAS summary datasets and the LD reference sample. SNPs with
      allele frequency differences greater than the specified threshold
      value will be excluded from the analysis. The default value is
      0.2.

  `cojo_gc`

  :   If this option is specified, p-values will be adjusted by the
      genomic control method. By default, the genomic inflation factor
      will be calculated from the summary-level statistics of all the
      SNPs unless you specify a value, e.g. `--cojo-gc 1.05`.

## Details

[**Documentation**](http://cnsgenomics.com/software/gcta/#COJO)\
Columns are SNP, the effect allele, the other allele, frequency of the
effect allele, effect size, standard error, p-value and sample size. The
headers are not keywords and will be omitted by the program. Important:
"A1" needs to be the effect allele with "A2" being the other allele and
"freq" should be the frequency of "A1".'\
Note: 1) For a case-control study, the effect size should be log(odds
ratio) with its corresponding standard error. 2) Please always input the
summary statistics of all SNPs even if your analysis only focuses on a
subset of SNPs because the program needs the summary data of all SNPs to
calculate the phenotypic variance. You can use one of the `--extract`
options (Data management) to limit the COJO analysis in a certain
genomic region.\
\
**General results columns**:\

- Chr:

  Chromosome.

- SNP:

  SNP RSID.

- bp:

  Physical position.

- refA:

  Effect allele.

- freq:

  Frequency of the effect allele in the original data.

- b:

  Effect size.

- se:

  Standard error.

- p:

  p-value from the original GWAS or meta-analysis.

- n:

  Estimated effective sample size.

- freq_geno:

  Frequency of the effect allele in the reference sample.

**Stepwise analysis results columns**:\

- bJ:

  Effect size from the joint analysis of all the selected SNPs.

- bJ_se:

  Standard error from the joint analysis of all the selected SNPs.

- pJ:

  p-value from the joint analysis of all the selected SNPs.

- LD_r:

  LD correlation between the SNP i and SNP i + 1 for the SNPs on the
  list.

- LD_r2:

  LD_r squared.

- CS:

  Whether the SNP is in the Credible Set, defined as any SNP with where
  `pJ<(1-credset_thresh)`.

**Conditional analysis results columns:**\

- bC:

  Effect size from the conditional analysis.

- bC_se:

  Standard error from the conditional analysis.

- pC:

  p-value from the conditional analysis.

- CS:

  Whether the SNP is in the Credible Set, defined as any SNP with where
  `pC<(1-credset_thresh)`.

## See also

Other COJO:
[`COJO_get_stepwise_results()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_get_stepwise_results.md),
[`COJO_locus_subdir()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_locus_subdir.md),
[`COJO_process_results()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_process_results.md),
[`COJO_run()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_run.md)

## Examples

``` r
if (FALSE) { # \dontrun{
vcf <- system.file("extdata", "BST1.1KGphase3.vcf.bgz",
    package = "echodata")
dat <- echodata::BST1
locus_dir <- file.path(tempdir(), echodata::locus_dir)
fullSS_path <- echodata::example_fullSS()
bfile <- echoLD::vcf_to_plink(vcf = vcf)$prefix
cojo_DT <- COJO(dat = dat,
                locus_dir = locus_dir,
                fullSS_path = fullSS_path,
                bfile = bfile)
} # }
```
