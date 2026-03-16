# Setup gcc

Set up a symlink to override xcode's clang with a valid version of gcc.
On Apple Silicon Macs, this checks both `/opt/homebrew` and `/usr/local`
Homebrew prefixes.

## Usage

``` r
setup_gcc(version = "latest", verbose = TRUE)
```

## Arguments

- version:

  gcc version to use (e.g. 7,8,9,10,14). If `"latest"`, the latest
  version of gcc currently installed on your machine will be used.

- verbose:

  Print messages.

## Value

A data.frame listing all gcc versions, their paths, and which one is
currently active.
