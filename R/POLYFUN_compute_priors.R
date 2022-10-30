#' PolyFun: recompute priors
#' 
#' Recompute SNP-wise priors from summary stats.
#' \strong{Important not on duplicate SNPs: } 
#' Make sure you have removed all duplicate SNPs from your
#' full summary stats file (\code{fullSS_path}) before running this function.
#' Or simply use the \code{remove_dup=TRUE} argument.
#' \strong{Important note on file formats:}
#' Make sure your full summary stats file (\code{fullSS_path}) is tab-delimited
#' (NOT space-delimited). This will be done automatically if you set
#'  \code{remove_dup=TRUE}.
#' @param sample_size Dataset sample size.
#' @param annotations_path Path prefix to annotation files 
#' #' (e.g. "path/to/files/annotations.").
#' If \code{NULL}, will simply use example files included with PolyFun.
#' @param weights_path Path prefix to weights files 
#' (e.g. "path/to/files/weights.").
#' If \code{NULL}, will simply use example files included with PolyFun.
#' @param prefix Dataset prefix name.
#' @param chrom Which chromosome to query.
#' @param compute_ldscores Whether to compute per-SNP heritability with
#' LDSCore regression.
#' @param allow_missing_SNPs Whether or not to allow missing SNPs.
#' @param ref_prefix Prefix of path leading to reference files.
#' @param remove_tmps Remove temporary files.
#' @param min_INFO Minimum per-SNP INFO criterion score.
#' @param min_MAF Minimum per-SNP MAF.
#' @param skip_ckmedian SKip ckmedian step.
#' @inheritParams multifinemap
#' @inheritParams POLYFUN
#' @inheritParams echoconda::activate_env
#' @family polyfun
#' 
#' @export
#' @importFrom echoconda activate_env find_python_path cmd_print
#' @examples
#' fullSS_path <- echodata::example_fullSS()
#' ldsc_files <- echofinemap:::POLYFUN_compute_priors(fullSS_path=fullSS_path)
POLYFUN_compute_priors <- function(fullSS_path,
                                   remove_dup=TRUE,
                                   polyfun_path=NULL,
                                   locus_dir=tempdir(),
                                   sample_size = NULL,
                                   min_INFO = 0,
                                   min_MAF = 0.001,
                                   skip_ckmedian=FALSE,
                                   num_bins=NULL, # 30 is reasonable
                                   annotations_path=NULL,
                                   weights_path=NULL,
                                   prefix="dataset1",
                                   chrom="all",
                                   compute_ldscores=FALSE,
                                   allow_missing_SNPs=TRUE,
                                   ref_prefix=NULL,
                                   remove_tmps=TRUE,
                                   conda_env = "echoR_mini",
                                   verbose=TRUE){
    
    # echoverseTemplate:::source_all()
    # echoverseTemplate:::args2vars(POLYFUN_compute_priors);
    
    conda_env <- echoconda::yaml_to_env(yaml_path = conda_env, 
                                         verbose = verbose)
    conda_env <- echoconda::activate_env(conda_env = conda_env,
                                         method = "basilisk")
    python <- echoconda::find_python_path(conda_env = conda_env)
    polyfun_path <- POLYFUN_find_folder(polyfun_path = polyfun_path)
    
    if(is.null(fullSS_path)){
        warning("No fullSS_path provided.",
                " Running PolyFun on example data only.")
        fullSS_path <- POLYFUN_example_data(type="sumstats")
    }
    if(is.null(annotations_path)){
        warning("No annotations_path provided.",
                " Running PolyFun on example data only.")
        annotations_path <- POLYFUN_example_data(type="annotations")
    }
    if(is.null(weights_path)){
        warning("No weights_path provided.",
                " Running PolyFun on example data only.")
        weights_path <- POLYFUN_example_data(type="weights")
    } 
    #### Remove duplicate SNPs ####
    fullSS_path <- remove_dup_snps(path = fullSS_path, 
                                   remove_dup = remove_dup, 
                                   verbose = verbose) 
    #### 0. Create paths ####
    PF.output.path <- file.path(locus_dir, "PolyFun")
    dir.create(PF.output.path, showWarnings = FALSE, recursive = TRUE)
    out.path <- file.path(PF.output.path,"output")
    dir.create(out.path, showWarnings = FALSE, recursive = TRUE)
    output_prefix <- file.path(out.path, prefix, prefix)
    dir.create(dirname(output_prefix), showWarnings = FALSE, recursive = TRUE)
    #### 1. Munge summary stats ####
    messager("PolyFun:: [1]  Create a munged summary statistics file",
             "in a PolyFun-friendly parquet format.",v=verbose)
    munged_path <- POLYFUN_munge_summ_stats(polyfun_path = polyfun_path,
                                            fullSS_path = fullSS_path,
                                            locus_dir = locus_dir,
                                            sample_size = sample_size,
                                            min_INFO = min_INFO,
                                            min_MAF = min_MAF,
                                            force_new_munge = FALSE,
                                            conda_env = conda_env, 
                                            verbose = verbose) 
    #### Run LDSC ####
    # NOTE! if you're running without the "--no-partitions" flag,
    ## you need to load R first `ml R`.
    messager("PolyFun:: [2] Run PolyFun with L2-regularized S-LDSC.",
             v=verbose) 
    polyfun_path <- "inst/tools/polyfun"
    cmd2 <- paste(python,
                  file.path(polyfun_path,"polyfun.py"),
                  "--compute-h2-L2",
                  # Approach 2 = Parametric = no partitions = T
                  # Approach 3 = Non-parametric = partitions = F
                  ifelse(compute_ldscores,"","--no-partitions"),
                  "--output-prefix",output_prefix,
                  "--sumstats",munged_path,
                  "--ref-ld-chr",annotations_path,
                  "--w-ld-chr",weights_path,
                  if(skip_ckmedian)"--skip-Ckmedian"else"",
                  if(!is.null(num_bins))paste("--num-bins",num_bins)else"",
                  ifelse(allow_missing_SNPs,"--allow-missing","")
                  )
    echoconda::cmd_print(cmd2, verbose = verbose)
    system(cmd2)
    
    # Computationally intensive: can parallelize by chromosomes
    if(isTRUE(compute_ldscores)){
        #### Get reference data if not provided ####
        if(is.null(ref_prefix)){
            ref_prefix <- POLYFUN_download_ref_files(conda_env = conda_env,
                                                     verbose = verbose)
        }
        # 3. Computationally intensive step
        messager("PolyFun:: [3] Compute LD-scores for each SNP bin.",
                 v=verbose)
        cmd3 <- paste(python,
                      file.path(polyfun_path,"polyfun.py"),
                      "--compute-ldscores",
                      "--output-prefix",output_prefix,
                      "--bfile-chr",ref_prefix,
                      ifelse(chrom=="all","",paste("--chr",chrom)),
                      ifelse(allow_missing_SNPs,"--allow-missing","") )
        echoconda::cmd_print(cmd3)
        system(cmd3)
        # 4.
        messager("PolyFun:: [4] Re-estimate per-SNP heritabilities via S-LDSC",
                 v=verbose)
        cmd4 <- paste(python,
                      file.path(polyfun_path,"polyfun.py"),
                      "--compute-h2-bins",
                      "--output-prefix",output_prefix,
                      "--sumstats",munged_path,
                      "--w-ld-chr",weights_path,
                      ifelse(allow_missing_SNPs,"--allow-missing",""))
        echoconda::cmd_print(cmd4)
        system(cmd4)
        
        messager("PolyFun:: Results directory =",dirname(output_prefix),
                 v=verbose)
        messager("PolyFun:: Results files:",v=verbose)
        messager("          *.snpvar_ridge.gz",v=verbose)
        messager("          *.snpvar_ridge_constrained.gz",v=verbose)
        ## The output of the PARTITIONED LDSC has the suffix: 
        ##.snpvar_constrained.gz (one per chrom)
        LDSC.files <- list.files(out.path,
                                 pattern = "*.snpvar_constrained.gz", 
                                 full.names = TRUE, 
                                 recursive = TRUE) 
    } else { 
        LDSC.files <- list.files(path = out.path,
                                 pattern = "_ridge_constrained.gz", 
                                 full.names = TRUE, 
                                 recursive = TRUE)
    }
    #### Return #####
    return(LDSC.files)
}




