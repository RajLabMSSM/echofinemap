# Find FINEMAP executable

Retrieve location of `FINEMAP` executable. First checks if `finemap` is
already available on the system PATH (e.g. installed via
`conda install -c bioconda finemap`), then falls back to downloading the
binary from GitHub releases. On Apple Silicon Macs, automatically sets
up x86_64 dynamic libraries via
[`FINEMAP_setup_dylibs`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_setup_dylibs.md).

## Usage

``` r
FINEMAP_find_executable(
  FINEMAP_path = NULL,
  OS = NULL,
  save_dir = file.path(tools::R_user_dir(package = "echofinemap", which = "cache"),
    "FINEMAP"),
  version = package_version("1.4.1"),
  verbose = TRUE
)
```

## Source

[FINEMAP site](http://www.christianbenner.com) [Similar dynlib error in
R](https://support.bioconductor.org/p/9141978/)

## See also

Other FINEMAP:
[`FINEMAP()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP.md),
[`FINEMAP_construct_data()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_data.md),
[`FINEMAP_construct_master()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_master.md),
[`FINEMAP_process_results()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_process_results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
FINEMAP_path <- echofinemap:::FINEMAP_find_executable(version="1.3.1")
} # }
```
