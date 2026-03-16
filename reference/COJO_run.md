# Run *GCTA-COJO*

Run *GCTA-COJO*.

## Usage

``` r
COJO_run(
  gcta_path,
  bfile,
  cojo_slct = TRUE,
  cojo_cond = TRUE,
  cojo_joint = FALSE,
  conditioned_snps = NULL,
  prefix = "cojo",
  cojo_dir,
  cojo_file = file.path(cojo_dir, paste0(prefix, ".file.ma")),
  exclude = file.path(cojo_dir, "excluded_snps.txt"),
  verbose = TRUE,
  ...
)
```

## Arguments

- gcta_path:

  Path to the GCTA-COJO executable.

- bfile:

  Input PLINK binary PED files, e.g. test.fam, test.bim and test.bed
  (see PLINK user manual for details).

- cojo_slct:

  Perform a stepwise model selection procedure to select independently
  associated SNPs. Results will be saved in a \*.jma file with
  additional file \*.jma.ldr showing the LD correlations between the
  SNPs.

- cojo_cond:

  Perform association analysis of the included SNPs conditional on the
  given list of SNPs. Results will be saved in a \*.cma. The conditional
  SNP effects (i.e. bC) will be labelled as "NA" if the multivariate
  correlation between the SNP in question and all the covariate SNPs is
  \> 0.9.

- cojo_joint:

  Fit all the included SNPs to estimate their joint effects without
  model selection. Results will be saved in a \*.jma file with
  additional file \*.jma.ldr showing the LD correlations between the
  SNPs.

- conditioned_snps:

  Which SNPs to conditions on when fine-mapping with (e.g. *COJO*).

- prefix:

  Prefix to use for file names.

- cojo_dir:

  Directory to store results in.

- cojo_file:

  Input the summary-level statistics from a meta-analysis GWAS (or a
  single GWAS).

- exclude:

  Specify a list of SNPs to be excluded from the analysis.

- verbose:

  Print messages.

- ...:

  Arguments passed on to
  [`COJO_args`](https://rajlabmssm.github.io/echofinemap/reference/COJO_args.md)

  `out`

  :   Specify output root filename.

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

## Value

Input and output file paths.

## See also

Other COJO:
[`COJO()`](https://rajlabmssm.github.io/echofinemap/reference/COJO.md),
[`COJO_get_stepwise_results()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_get_stepwise_results.md),
[`COJO_locus_subdir()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_locus_subdir.md),
[`COJO_process_results()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_process_results.md)
