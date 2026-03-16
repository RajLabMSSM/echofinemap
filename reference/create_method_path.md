# Create method path

Create a fine-mapping method-specific path.

## Usage

``` r
create_method_path(
  locus_dir,
  finemap_method,
  include_astrices = FALSE,
  LD_reference = NULL,
  compress = FALSE
)
```

## Arguments

- locus_dir:

  Locus-specific directory to store results in.

- finemap_method:

  Fine-mapping method to run. See
  [lfm](https://rajlabmssm.github.io/echofinemap/reference/lfm.md) for a
  list of all fine-mapping methods currently available.

- include_astrices:

  Whether to keep any astrices in file path.

- LD_reference:

  Name of the LD reference panel.

- compress:

  Whether to add ".gz" at the end of the file path.

## See also

Other finemapping functions:
[`multifinemap()`](https://rajlabmssm.github.io/echofinemap/reference/multifinemap.md),
[`multifinemap_handler()`](https://rajlabmssm.github.io/echofinemap/reference/multifinemap_handler.md),
[`multifinemap_handler_method()`](https://rajlabmssm.github.io/echofinemap/reference/multifinemap_handler_method.md)

## Examples

``` r
if (FALSE) { # \dontrun{
locus_dir <- echodata::locus_dir
path <- echofinemap::create_method_path(locus_dir = locus_dir, 
                                        finemap_method = "SUSIE")
} # }
```
