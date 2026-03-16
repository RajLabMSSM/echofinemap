# Get data

Get data via piggyback.

## Usage

``` r
get_data(
  fname,
  repo = "RajLabMSSM/echofinemap",
  save_dir = tempdir(),
  tag = "latest",
  overwrite = FALSE,
  ...
)
```

## Arguments

- fname:

  File name.

- repo:

  GitHub repository name.

- save_dir:

  Local directory to cache data in.

- tag:

  tag for the GitHub release to which this data should be attached.

- overwrite:

  Should any local files of the same name be overwritten? default
  `TRUE`.

- ...:

  Arguments passed on to
  [`echodata::get_data`](https://rdrr.io/pkg/echodata/man/get_data.html)

  :   
