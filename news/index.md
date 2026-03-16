# Changelog

## echofinemap 1.0.0

### New features

- `FINEMAP`: Added early runnability check via
  [`FINEMAP_check_runnable()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_check_runnable.md)
  — validates the binary can execute before preparing data, with
  platform-specific error messages for Apple Silicon dylib issues.
- `FINEMAP`: Added automatic x86_64 dynamic library setup on Apple
  Silicon via
  [`FINEMAP_setup_dylibs()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_setup_dylibs.md).
  Builds/downloads x86_64 libzstd, libomp, and gcc runtime libraries,
  then rewrites binary paths with `install_name_tool`.
- `FINEMAP`: System PATH detection — checks for conda-installed FINEMAP
  before downloading from GitHub releases.
- `PAINTOR`: Wired functional annotation pipeline end-to-end.
  [`PAINTOR_prepare_annotations()`](https://rajlabmssm.github.io/echofinemap/reference/PAINTOR_prepare_annotations.md)
  is a pure-R reimplementation of the original Python 2.7
  `AnnotateLocus.py`, converting BED files to binary annotation matrices
  and passing annotation names to the PAINTOR executable.
- `PAINTOR`: New `annot_paintor` parameter for downloading the
  comprehensive PAINTOR functional annotation library (~7GB, 10,000+
  annotations) from BOX. Annotations are cached locally after first
  download.
- `PAINTOR`: New
  [`PAINTOR_fetch_annotations()`](https://rajlabmssm.github.io/echofinemap/reference/PAINTOR_fetch_annotations.md)
  and
  [`PAINTOR_list_annotations()`](https://rajlabmssm.github.io/echofinemap/reference/PAINTOR_list_annotations.md)
  functions for managing the annotation library.
- `POLYFUN`: Fixed parametric mode filename pattern — parametric output
  has no chromosome in the filename, so the import now matches
  correctly.

### Bug fixes

- `FINEMAP`: Fixed sparse matrix (`dsCMatrix`) handling in
  [`FINEMAP_construct_data()`](https://rajlabmssm.github.io/echofinemap/reference/FINEMAP_construct_data.md)
  by converting to dense matrix before `fwrite()`.
- `POLYFUN`: Fixed silent error swallowing in
  [`POLYFUN_import_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_import_priors.md)
  — replaced `try({})` with proper exit code checking and output file
  verification.
- `POLYFUN`: Fixed
  [`requireNamespace("Ckmeans.1d.dp")`](https://rdrr.io/r/base/ns-load.html)
  to properly error with an informative message instead of silently
  continuing.
- `POLYFUN`: Fixed
  [`POLYFUN_install()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_install.md)
  blocking non-interactive sessions by gating
  [`readline()`](https://rdrr.io/r/base/readline.html) behind
  [`interactive()`](https://rdrr.io/r/base/interactive.html) check.
- `POLYFUN`: Removed hardcoded path override in
  [`POLYFUN_compute_priors()`](https://rajlabmssm.github.io/echofinemap/reference/POLYFUN_compute_priors.md)
  that was overwriting the properly resolved polyfun path.
- `PAINTOR`: Removed `setup_gcc(version="clang")` call from
  [`PAINTOR_install()`](https://rajlabmssm.github.io/echofinemap/reference/PAINTOR_install.md)
  that required sudo for symlink creation.
- [`setup_gcc()`](https://rajlabmssm.github.io/echofinemap/reference/setup_gcc.md):
  Rewritten for Apple Silicon support — checks both `/opt/homebrew`
  (ARM) and `/usr/local` (Intel) Homebrew paths, handles multiple gcc
  installation formats.
- Tests: Split monolithic `test-multifinemap.R` into focused subtests
  (BST1, LRRK2 with network skip, POLYFUN_SUSIE with dependency checks).
- Tests: Added proper skip guards for FINEMAP, PAINTOR, and POLYFUN
  tests with direct function checks instead of tryCatch wrappers.
- Local R CMD check fixes and compatibility updates.

## echofinemap 0.99.7

### Bug fixes

- Local R CMD check fixes and compatibility updates.

## echofinemap 0.99.6

### New features

- Standardize `rworkflows.yml` with canonical template: enable Docker on
  `ghcr.io`, set `write-all` permissions, use `GITHUB_TOKEN`, add
  `devel` branch trigger.

## echofinemap 0.99.5

### New features

- Implement `rworkflows`

## echofinemap 0.99.4

### New features

- Pass more arg down to method-level via `finemap_args`.
- Add `check_args` function to handle `finemap_args`.
- Offload `piggyback` functions to echodata
  ([`echodata::get_data`](https://rdrr.io/pkg/echodata/man/get_data.html))
- `SUSIE`:
  - Condense `manual_var_y` and `var_name` into one arg: `var_y`
  - Replace `dataset_type` with more appropriate `case_control` arg.
- Fully documented all functions.
- Remove `POLYFUN_plot` (redundant code with `echoplot`)
- `POLYFUN_`: fixed or deleted functions
  - `POLYFUN_munge_summ_stats`
  - `POLYFUN_compute_priors`

### Bug fixes

- Make `Rfast` an Import to avoid downstream issues with `echolocatoR`.
- Fix GHA: [@master](https://github.com/master) –\>
  [@v2](https://github.com/v2)\
- Remove unnecessary deps:
  - `ggplot2`
- Adjust *.Rbuildignore* to not cause issues with large `inst/tools`
  subfolder.
- `require_cols`:
  - Now takes the more accurate `case_control` arg instead of
    `dataaset_type`.

## echofinemap 0.99.3

### New features

- `COJO`:
  - Now working with *echoverse*.
  - Added 3 COJO modes to list of finemapping methods:
    `run_stepwise`,`run_conditional`,`run_joint`
  - Automated installation of GCTA.
  - Added unit tests.
- `PAINTOR`:
  - Now working with *echoverse*.
  - Generalized to any data input type (GWAS/QTL/TWAS) to avoid
    hard-coded args.
  - Streamlined functions and reduced clutter.\
  - Automated installation of PAINTOR.
  - Can now handle multi-trait and multi-ancestry fine-mapping.
  - Added unit tests.
- `multifinemap`:
  - Automatically checks `finemap_methods`
  - Passes `seed` all the way to finemapping functions.
  - Expanded finemapping methods.
- `check_required_cols`: Add coloring via `cli`.
- `required_cols`: Add URLS to GitHub repos and citations.
- Added functions for checking each fine-mapping CLI method’s version:
  - `<method>_check_version`
  - Changed `POLYFUN_find_polyfun_folder` –\> `POLYFUN_find_folder` for
    consistency.

### Bug fixes

- `FINEMAP`: Figured out solution to get FINEMAP v1.4 working on Mac.
  Instructions in messages now updated.
- `create_method_path`: Use `basename(LD_reference)` for file naming.
- Require “MAF” columns for coloc, as it now seems to be required by the
  error message when it’s missing:
  `dataset : please give MAF if using p values`

## echofinemap 0.99.2

### New features

- Updated GHA.
- `POLYFUN`:
  - Can now use `FINEMAP` or `SUSIE` as a method.
  - Change column “POLYFUN_h2” –\> “POLYFUN.h2” to make consistent with
    other cols.
  - Automatically installs itself from the GitHub submodule.
  - `POLYFUN_help` now exported.
- Use `Rfast` to speed up `SUSIE`.
- `remove_na_rows`: Remove rows with NAs in tool-specific essential
  cols.
- Update `FINEMAP` to v1.4.1.
- Make separate columns for `FINEMAP` results:
  - *PP*: Conditional PP (from .cred file(s))
  - *PP_snp*: Marginal PP (from .snp file)
  - *PP_config*: Configuration PP (from .config file)
  - *k*: Optimized number of causal SNPs (\<= `n_causal`).\
- Got `FINEMAP` to use priors via `data.k` file.

### Bug fixes

- Switch SUSIE method from `susie_suff_stat` to `susie_rss` for
  parameter consistency.
- Fix `ABF`.
  - Use mean `proportion_cases`
- Test `FINEMAP` v1.4 and v1.3.1 on Linux.
- Actually pass `force_new_finemap` all the way down.
- Dynamically infer “data.cred\*” file name.
- Fill NAs in FINEMAP CS/PP.
- `SUSIE`:
  - `susieR` v0.11 contained serious bugs that caused `susie_rss` to
    fail. This was fixed in v0.12 (which is now a version requirement
    for `echofinemap`). See here for details:
    <https://github.com/stephenslab/susieR/issues/167>

## echofinemap 0.99.1

### New features

- Updated conda env to *echoR_mini*.
- Add `prior_k` as argument to `FINEMAP`.
- Add message about gcc8 to FINEMAP v1.4 error.

### Bug fixes

- Minor fixes to POLYFUN method checks.

## echofinemap 0.99.0

- Added a `NEWS.md` file to track changes to the package.
