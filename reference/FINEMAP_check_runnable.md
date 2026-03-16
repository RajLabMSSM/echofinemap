# Check FINEMAP is runnable

Test that the FINEMAP binary can actually execute on the current system.
On Apple Silicon Macs, the x86_64 FINEMAP binary requires Rosetta 2 and
x86_64 versions of several dynamic libraries (zstd, gcc, libomp).

## Usage

``` r
FINEMAP_check_runnable(FINEMAP_path, verbose = TRUE)
```

## Arguments

- FINEMAP_path:

  Path to the FINEMAP executable.

- verbose:

  Print messages.

## Value

`TRUE` if FINEMAP runs successfully, otherwise stops with an informative
error message.
