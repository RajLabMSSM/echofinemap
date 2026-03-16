# echofinemap: Getting Started

``` r

library(echofinemap)
```

## Quick start: ABF (Approximate Bayes Factor)

ABF is the fastest fine-mapping method because it requires no LD matrix
and no external software. It is a good first pass for identifying
credible sets.

``` r

## Load bundled example data
dat <- echodata::BST1
LD_matrix <- echodata::BST1_LD_matrix

## Strip any previous fine-mapping columns so we start fresh
dat_clean <- echofinemap::drop_finemap_cols(dat = dat)
```

    ## Gathering method sources.

    ## Gathering method citations.

    ## Dropping 11 pre-existing fine-mapping columns.

``` r

## Run ABF
dat_abf <- echofinemap::ABF(dat = dat_clean, verbose = TRUE)
```

    ## Loading required namespace: MungeSumstats

    ## Preparing sample size column (N).

    ## Computing effective sample size using the LDSC method:
    ##  Neff = (N_CAS+N_CON) * (N_CAS/(N_CAS+N_CON)) / mean((N_CAS/(N_CAS+N_CON))[(N_CAS+N_CON)==max(N_CAS+N_CON)]))

    ## + Mapping colnames from MungeSumstats ==> echolocatoR

    ## Running ABF.

    ## Warning in .maybe_warn_merge_dots(...): merge.data.table() received 1 unknown
    ## keyword argument which will be ignored: [on]

Inspect the results – ABF adds posterior probability columns and a
credible set indicator:

``` r

## Key fine-mapping columns added by ABF
abf_cols <- grep("ABF|PP|CS", names(dat_abf), value = TRUE)
print(abf_cols)
```

    ## [1] "mean.CS" "PP"      "CS"

``` r

## Top SNPs by ABF posterior probability
topsnps <- head(dat_abf[order(-dat_abf$PP),
                         c("SNP","CHR","POS","P","PP","CS")], 10)
print(topsnps)
```

    ## Key: <SNP>
    ##            SNP   CHR      POS         P           PP    CS
    ##         <char> <int>    <int>     <num>        <num> <num>
    ##  1:  rs4698412     4 15737348 2.058e-28 3.870528e-01     0
    ##  2: rs11724635     4 15737101 2.832e-28 3.064438e-01     0
    ##  3:  rs4698413     4 15737882 3.849e-28 2.427324e-01     0
    ##  4: rs34559912     4 15730146 1.213e-27 6.211324e-02     0
    ##  5:  rs4613561     4 15737890 1.039e-25 1.411516e-03     0
    ##  6:  rs6449168     4 15727713 1.766e-24 1.298016e-04     0
    ##  7:  rs4266290     4 15737118 3.111e-24 4.631918e-05     0
    ##  8:  rs4403048     4 15737120 3.057e-24 4.631918e-05     0
    ##  9:  rs4263397     4 15739390 1.175e-23 2.058995e-05     0
    ## 10:  rs4631042     4 15712550 8.208e-23 1.318113e-06     0

## Full pipeline: `multifinemap()`

[`multifinemap()`](https://rajlabmssm.github.io/echofinemap/reference/multifinemap.md)
runs one or more fine-mapping methods in a single call. The example
below runs ABF, SUSIE, and FINEMAP together.

> **Note:** This chunk is set to `eval=FALSE` because SUSIE and FINEMAP
> can be slow and FINEMAP requires an external binary. See the
> individual method help pages for installation instructions.

``` r

dat_clean <- echofinemap::drop_finemap_cols(dat = echodata::BST1)
LD_matrix <- echodata::BST1_LD_matrix
locus_dir <- file.path(tempdir(), echodata::locus_dir)

dat2 <- echofinemap::multifinemap(
    dat = dat_clean,
    locus_dir = locus_dir,
    LD_matrix = LD_matrix,
    finemap_methods = c("ABF", "SUSIE", "FINEMAP")
)
```

## Session Info

``` r

utils::sessionInfo()
```

    ## R Under development (unstable) (2026-03-12 r89607)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] echofinemap_1.0.0 BiocStyle_2.39.0 
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] splines_4.6.0               aws.s3_0.3.22              
    ##   [3] BiocIO_1.21.0               bitops_1.0-9               
    ##   [5] filelock_1.0.3              tibble_3.3.1               
    ##   [7] R.oo_1.27.1                 cellranger_1.1.0           
    ##   [9] basilisk.utils_1.23.1       graph_1.89.1               
    ##  [11] rpart_4.1.24                XML_3.99-0.22              
    ##  [13] lifecycle_1.0.5             mixsqp_0.3-54              
    ##  [15] OrganismDbi_1.53.2          ensembldb_2.35.0           
    ##  [17] lattice_0.22-9              MASS_7.3-65                
    ##  [19] backports_1.5.0             magrittr_2.0.4             
    ##  [21] Hmisc_5.2-5                 openxlsx_4.2.8.1           
    ##  [23] sass_0.4.10                 rmarkdown_2.30             
    ##  [25] jquerylib_0.1.4             yaml_2.3.12                
    ##  [27] otel_0.2.0                  zip_2.3.3                  
    ##  [29] ggbio_1.59.0                reticulate_1.45.0          
    ##  [31] gld_2.6.8                   DBI_1.3.0                  
    ##  [33] RColorBrewer_1.1-3          abind_1.4-8                
    ##  [35] expm_1.0-0                  GenomicRanges_1.63.1       
    ##  [37] purrr_1.2.1                 R.utils_2.13.0             
    ##  [39] AnnotationFilter_1.35.0     biovizBase_1.59.0          
    ##  [41] BiocGenerics_0.57.0         RCurl_1.98-1.17            
    ##  [43] nnet_7.3-20                 VariantAnnotation_1.57.1   
    ##  [45] IRanges_2.45.0              S4Vectors_0.49.0           
    ##  [47] echoLD_1.0.0                irlba_2.3.7                
    ##  [49] pkgdown_2.2.0               echodata_1.0.0             
    ##  [51] piggyback_0.1.5             codetools_0.2-20           
    ##  [53] DelayedArray_0.37.0         DT_0.34.0                  
    ##  [55] xml2_1.5.2                  tidyselect_1.2.1           
    ##  [57] UCSC.utils_1.7.1            farver_2.1.2               
    ##  [59] viridis_0.6.5               matrixStats_1.5.0          
    ##  [61] stats4_4.6.0                base64enc_0.1-6            
    ##  [63] Seqinfo_1.1.0               echotabix_1.0.1            
    ##  [65] GenomicAlignments_1.47.0    jsonlite_2.0.0             
    ##  [67] e1071_1.7-17                Formula_1.2-5              
    ##  [69] survival_3.8-6              systemfonts_1.3.2          
    ##  [71] tools_4.6.0                 ragg_1.5.1                 
    ##  [73] DescTools_0.99.60           Rcpp_1.1.1                 
    ##  [75] glue_1.8.0                  gridExtra_2.3              
    ##  [77] SparseArray_1.11.11         xfun_0.56                  
    ##  [79] MatrixGenerics_1.23.0       GenomeInfoDb_1.47.2        
    ##  [81] dplyr_1.2.0                 withr_3.0.2                
    ##  [83] BiocManager_1.30.27         fastmap_1.2.0              
    ##  [85] basilisk_1.23.0             boot_1.3-32                
    ##  [87] digest_0.6.39               R6_2.6.1                   
    ##  [89] colorspace_2.1-2            textshaping_1.0.5          
    ##  [91] dichromat_2.0-0.1           RSQLite_2.4.6              
    ##  [93] cigarillo_1.1.0             R.methodsS3_1.8.2          
    ##  [95] tidyr_1.3.2                 generics_0.1.4             
    ##  [97] data.table_1.18.2.1         rtracklayer_1.71.3         
    ##  [99] class_7.3-23                httr_1.4.8                 
    ## [101] htmlwidgets_1.6.4           S4Arrays_1.11.1            
    ## [103] pkgconfig_2.0.3             gtable_0.3.6               
    ## [105] Exact_3.3                   blob_1.3.0                 
    ## [107] S7_0.2.1                    XVector_0.51.0             
    ## [109] echoconda_1.0.0             htmltools_0.5.9            
    ## [111] susieR_0.14.2               bookdown_0.46              
    ## [113] RBGL_1.87.0                 ProtGenerics_1.43.0        
    ## [115] scales_1.4.0                Biobase_2.71.0             
    ## [117] lmom_3.2                    png_0.1-8                  
    ## [119] knitr_1.51                  rstudioapi_0.18.0          
    ## [121] reshape2_1.4.5              tzdb_0.5.0                 
    ## [123] rjson_0.2.23                checkmate_2.3.4            
    ## [125] curl_7.0.0                  proxy_0.4-29               
    ## [127] cachem_1.1.0                stringr_1.6.0              
    ## [129] rootSolve_1.8.2.4           parallel_4.6.0             
    ## [131] foreign_0.8-91              AnnotationDbi_1.73.0       
    ## [133] restfulr_0.0.16             desc_1.4.3                 
    ## [135] pillar_1.11.1               grid_4.6.0                 
    ## [137] reshape_0.8.10              vctrs_0.7.1                
    ## [139] cluster_2.1.8.2             htmlTable_2.4.3            
    ## [141] evaluate_1.0.5              readr_2.2.0                
    ## [143] GenomicFeatures_1.63.1      mvtnorm_1.3-5              
    ## [145] cli_3.6.5                   compiler_4.6.0             
    ## [147] Rsamtools_2.27.1            rlang_1.1.7                
    ## [149] crayon_1.5.3                aws.signature_0.6.0        
    ## [151] ieugwasr_1.1.0              plyr_1.8.9                 
    ## [153] forcats_1.0.1               fs_1.6.7                   
    ## [155] stringi_1.8.7               coloc_5.2.3                
    ## [157] echoannot_1.0.1             viridisLite_0.4.3          
    ## [159] BiocParallel_1.45.0         Biostrings_2.79.5          
    ## [161] lazyeval_0.2.2              Matrix_1.7-4               
    ## [163] downloadR_1.0.0             dir.expiry_1.19.0          
    ## [165] MungeSumstats_1.19.5        BSgenome_1.79.1            
    ## [167] patchwork_1.3.2             hms_1.1.4                  
    ## [169] bit64_4.6.0-1               ggplot2_4.0.2              
    ## [171] KEGGREST_1.51.1             SummarizedExperiment_1.41.1
    ## [173] haven_2.5.5                 memoise_2.0.1              
    ## [175] snpStats_1.61.1             bslib_0.10.0               
    ## [177] bit_4.6.0                   readxl_1.4.5
