# Prepare PAINTOR annotations

Convert BED annotation files into a PAINTOR-format annotation matrix.
For each SNP in the locus file, checks whether its position falls within
any interval in each BED file, producing a binary (0/1) matrix.

## Usage

``` r
PAINTOR_prepare_annotations(
  BED_paths,
  dat_merged,
  PT_results_path,
  locus_dir,
  verbose = TRUE
)
```

## Arguments

- BED_paths:

  Character vector of paths to BED files (can be gzipped). Each BED file
  becomes one annotation column.

- dat_merged:

  data.table with at least `CHR` and `POS` columns containing the SNPs
  to annotate.

- PT_results_path:

  Directory where the annotation matrix will be written.

- locus_dir:

  Locus directory (used for naming the output file).

- verbose:

  Print messages.

## Value

Path to the annotation matrix file.

## Details

This is a pure-R reimplementation of the original `AnnotateLocus.py`
Python 2.7 utility bundled with PAINTOR.
