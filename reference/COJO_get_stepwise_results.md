# Gather stepwise conditional results

Gather and preprocess the results of the *GCTA-COJO* conditional
stepwise procedure. [**GTCA-COJO
Documentation**:](https://yanglab.westlake.edu.cn/software/gcta)\
"Perform a stepwise model selection procedure to select independently
associated SNPs. Results will be saved in a *.jma* file with additional
file *.jma.ldr* showing the LD correlations between the SNPs."

## Usage

``` r
COJO_get_stepwise_results(
  cojo_dir = NULL,
  prefix = "cojo",
  jma_cojo_path = file.path(cojo_dir, paste0(prefix, ".jma.cojo")),
  verbose = TRUE
)
```

## Arguments

- prefix:

  Prefix to use for file names.

- verbose:

  Print messages.

## Value

independent_snps

## References

<https://www.nature.com/articles/ng.2213>
<https://www.cell.com/ajhg/fulltext/S0002-9297(10)00598-7>
<https://cnsgenomics.com/software/gcta/#Overview>

## See also

Other COJO:
[`COJO()`](https://rajlabmssm.github.io/echofinemap/reference/COJO.md),
[`COJO_locus_subdir()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_locus_subdir.md),
[`COJO_process_results()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_process_results.md),
[`COJO_run()`](https://rajlabmssm.github.io/echofinemap/reference/COJO_run.md)
