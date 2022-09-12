#' Recompute SNP-wise priors from summary stats
#' @keywords internal
#' @family polyfun
#' @examples
#' \dontrun{
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' fullSS_path <- echodata::example_fullSS()
#' LDSC.files <- POLYFUN_compute_priors(locus_dir=locus_dir,
#'                                      fullSS_path=fullSS_path)
#' }
POLYFUN_compute_priors <- function(polyfun=NULL,
                                   locus_dir,
                                   fullSS_path,
                                   sample_size = NULL,
                                   min_INFO = 0,
                                   min_MAF = 0.001,
                                   annotations_path=NULL,
                                   weights_path=NULL,
                                   prefix="GWAS1",
                                   chrom="all",
                                   compute_ldscores=FALSE,
                                   allow_missing_SNPs=TRUE,
                                   ref_prefix=NULL,
                                   remove_tmps=TRUE,
                                   conda_env = "echoR_mini"){
    # echoverseTemplate:::args2vars(POLYFUN_compute_priors); 
    # echoverseTemplate:::source_all()
    
    python <- echoconda::find_python_path(conda_env = conda_env)
    polyfun <- POLYFUN_find_folder(polyfun_path = polyfun)
    
    if(is.null(annotations_path)){
        warning("No annotations_path provided.",
                " Running PolyFun on example data only.")
        annotations_path <- file.path(
            system.file("tools/polyfun/example_data",
                        package="echofinemap"),
            "annotations.")
    }
    if(is.null(weights_path)){
        warning("No weights_path provided.",
                " Running PolyFun on example data only.")
        weights_path <- file.path(
            system.file("tools/polyfun/example_data",
                        package="echofinemap"),
            "weights.")
    }
    #### 0. Create paths ####
    PF.output.path <- file.path(locus_dir, "PolyFun")
    dir.create(PF.output.path, showWarnings = FALSE, recursive = TRUE)
    out.path <- file.path(PF.output.path,"output")
    output_prefix <- file.path(out.path, prefix, prefix)
    dir.create(out.path, showWarnings = FALSE, recursive = TRUE)
    
    
    #### 1. Munge summary stats ####
    messager("PolyFun:: [1]  Create a munged summary statistics file",
             "in a PolyFun-friendly parquet format.")
    munged.path <- POLYFUN_munge_summ_stats(polyfun=polyfun,
                                            fullSS_path = fullSS_path,
                                            locus_dir=locus_dir,
                                            sample_size=sample_size,
                                            min_INFO = min_INFO,
                                            min_MAF = min_MAF,
                                            force_new_munge = FALSE,
                                            conda_env = conda_env)
    
    # 2.
    ## If compute_ldscores == F:
    # This will create 2 output files for each chromosome: output/testrun.<CHR>.snpvar_ridge.gz and output/testrun.<CHR>.snpvar_ridge_constrained.gz. The first contains estimated per-SNP heritabilities for all SNPs (which can be used for downstream analysis with PolyFun; see below), and the second contains truncated per-SNP heritabilities, which can be used directly as prior causal probabilities in fine-mapping.
    # library(reticulate)
    # reticulate::use_virtualenv("echoR_mini")
    # pd <- reticulate::import("pandas")
    # pd$read_csv("./Data/directories_table.csv")
    # reticulate::
    # source_python(file.path(polyfun,"polyfun.py"))
    
    # NOTE! if you're running without the "--no-partitions" flag,
    ## you need to load R first `ml R`.
    messager("PolyFun:: [2] Run PolyFun with L2-regularized S-LDSC")
    # pf <- reticulate::py_run_file(polyfun)
    # reticulate::py_call()
    
    cmd2 <- paste(python,
                  file.path(polyfun,"polyfun.py"),
                  "--compute-h2-L2",
                  # Approach 2 = Parametric = no partitions = T
                  # Approach 3 = Non-parametric = partitions = F
                  ifelse(compute_ldscores,"","--no-partitions"),
                  "--output-prefix",output_prefix,
                  "--sumstats",munged_path,
                  "--ref-ld-chr",annotations_path,
                  "--w-ld-chr",weights_path,
                  ifelse(allow_missing_SNPs,"--allow-missing",""))
    echoconda::cmd_print(cmd2)
    system2(cmd2)
    
    # Computationally intensive: can parallelize by chromosomes
    if(compute_ldscores){
        # 3. Computationally intensive step
        messager("PolyFun:: [3] Compute LD-scores for each SNP bin")
        cmd3 <- paste(python,
                      file.path(polyfun,"polyfun.py"),
                      "--compute-ldscores",
                      "--output-prefix",output_prefix,
                      "--bfile-chr",ref_prefix,
                      ifelse(chrom=="all","",paste("--chr",chrom)),
                      ifelse(allow_missing_SNPs,"--allow-missing","") )
        echoconda::cmd_print(cmd3)
        system2(cmd3)
        # 4.
        messager("PolyFun:: [4] Re-estimate per-SNP heritabilities via S-LDSC")
        cmd4 <- paste(python,
                      file.path(polyfun,"polyfun.py"),
                      "--compute-h2-bins",
                      "--output-prefix",output_prefix,
                      "--sumstats",munged_path,
                      "--w-ld-chr",weights_path,
                      ifelse(allow_missing_SNPs,"--allow-missing",""))
        echoconda::cmd_print(cmd4)
        system(cmd4)
        
        messager("PolyFun:: Results directory =",dirname(output_prefix))
        messager("PolyFun:: Results files:")
        messager("          *.snpvar_ridge.gz")
        messager("          *.snpvar_ridge_constrained.gz")
        # The output of the PARTITIONED LDSC has the suffix: .snpvar_constrained.gz (one per chrom)
        LDSC.files <- list.files(out.path,
                                 pattern = "*.snpvar_constrained.gz", full.names = TRUE)
        # pd_ldsc <- data.table::fread(PS_LDSC.files[1], nThread = 1)
        # ldscore <- echodata::read_parquet(file.path(out.path,"PD_GWAS.1.l2.ldscore.parquet"))
        # bin.1 <- echodata::read_parquet(file.path(out.path,"PD_GWAS.2.bins.parquet"))
        #rowSums(bin.1[,-c(1:5)]) # each SNP belongs to only 1 bin
    } else { LDSC.files <- list.files(out.path, pattern = "_ridge_constrained.gz", full.names = TRUE) }
    
    
    
    if(remove_tmps){ file.remove(snp.path) }
    return(LDSC.files)
}




