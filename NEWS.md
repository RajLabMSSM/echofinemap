# echofinemap 0.99.2

## New features

* Updated GHA. 
* `POLYFUN`:
    - Can now use `FINEMAP` or `SUSIE` as a method. 
    - Change column "POLYFUN_h2" --> "POLYFUN.h2" 
    to make consistent with other cols. 
* Use `Rfast` to speed up `SUSIE`.
* `remove_na_rows`: Remove rows with NAs in tool-specific essential cols.
* Update `FINEMAP` to v1.4.1. 
* Make separate columns for `FINEMAP` results:
    - *PP*: Conditional PP (from .cred file(s))
    - *PP_snp*: Marginal PP (from .snp file)
    - *PP_config*: Configuration PP (from .config file)
    - *k*: Optimized number of causal SNPs (<= `n_causal`).  
* Got `FINEMAP` to use priors via `data.k` file. 

## Bug fixes

* Switch SUSIE method from `susie_suff_stat` to `susie_rss` for parameter consistency. 
* Fix `ABF`.
    - Use mean `proportion_cases`
* Test `FINEMAP` v1.4 and v1.3.1 on Linux. 
* Actually pass `force_new_finemap` all the way down. 
* Dynamically infer "data.cred*" file name. 
* Fill NAs in FINEMAP CS/PP. 
* `SUSIE`: 
    - `susieR` v0.11 contained serious bugs that caused `susie_rss` to fail. 
    This was fixed in v0.12 (which is now a version requirement for `echofinemap`). 
    See here for details:  https://github.com/stephenslab/susieR/issues/167 

# echofinemap 0.99.1

## New features

* Updated conda env to *echoR_mini*. 
* Add `prior_k` as argument to `FINEMAP`.
* Add message about gcc8 to FINEMAP v1.4 error.

## Bug fixes

* Minor fixes to POLYFUN method checks. 

# echofinemap 0.99.0

* Added a `NEWS.md` file to track changes to the package.
