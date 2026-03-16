# Setup FINEMAP dynamic libraries

On Apple Silicon Macs, the pre-compiled x86_64 FINEMAP binary requires
x86_64 versions of several dynamic libraries (zstd, gcc runtime,
libomp). This function downloads and compiles the required libraries,
then uses `install_name_tool` to rewrite the binary's library paths to
point to the local copies.

## Usage

``` r
FINEMAP_setup_dylibs(FINEMAP_path, verbose = TRUE)
```

## Arguments

- FINEMAP_path:

  Path to the FINEMAP executable.

- verbose:

  Print messages.

## Value

The (potentially modified) FINEMAP path, invisibly.
