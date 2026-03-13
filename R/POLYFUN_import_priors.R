#' Import pre-computed priors
#'
#' Import SNP-wise prior probabilities pre-computed from many UK Biobank traits.
#' This function handles finding the intersection of SNPS that exist in the
#' input GWAS summary stats \code{dat} and the pre-computed priors that come
#' shipped with PolyFun. Then, it saves this subset as a new file for PolyFun
#' (or other fine-mapping tools) to use as input.
#' Uses the \code{extract_snpvar.py} script from PolyFun.
#' @source
#' https://www.nature.com/articles/s41588-020-00735-5
#' @keywords internal
#' @family polyfun
#' @importFrom echoconda cmd_print
POLYFUN_import_priors <- function(locus_dir,
                                  dat=NULL,
                                  polyfun=NULL,
                                  force_new_priors=TRUE,
                                  remove_tmps=FALSE,
                                  nThread=1,
                                  conda_env="echoR_mini",
                                  verbose=TRUE){

    python <- echoconda::find_python_path(conda_env = conda_env,
                                          verbose = verbose)
    polyfun <- POLYFUN_find_folder(polyfun_path = polyfun)
    PF.output.path <- POLYFUN_initialize(locus_dir=locus_dir)
    snp_w_priors.file <- file.path(PF.output.path,
                                   "snps_with_priors.snpvar.tsv.gz")

    if(file.exists(snp_w_priors.file) && isFALSE(force_new_priors)){
        #### Import existing priors ####
        priors <- POLYFUN_read_priors(snp_w_priors.file = snp_w_priors.file,
                                      nThread = nThread,
                                      verbose = verbose)
        return(priors)
    }

    if(is.null(dat)) stop("dat is a required argument when priors must be computed.")

    ## [1]. Prepare input
    snp.path <- POLYFUN_prepare_snp_input(PF.output.path=PF.output.path,
                                          locus_dir=locus_dir,
                                          dat=dat)
    ## [2.] Retrieve priors via extract_snpvar.py
    extract_script <- file.path(polyfun, "extract_snpvar.py")
    if(!file.exists(extract_script)){
        stop("PolyFun extract_snpvar.py not found at: ", extract_script,
             "\nIs the PolyFun submodule installed?")
    }
    cmd <- paste(python,
                 extract_script,
                 "--sumstats", snp.path,
                 "--out", snp_w_priors.file)
    echoconda::cmd_print(cmd, verbose = verbose)
    exit_code <- system(cmd)
    if(exit_code != 0){
        stop("PolyFun extract_snpvar.py failed with exit code ", exit_code,
             ".\nCommand: ", cmd)
    }

    ## [3.] Handle missing SNPs
    miss.file <- paste0(snp_w_priors.file, ".miss.gz")
    if(file.exists(miss.file)){
        messager("+ PolyFun:: Rerunning after removing missing SNPs.",
                 v = verbose)
        miss.snps <- data.table::fread(miss.file, nThread = nThread)
        filt_DT <- subset(dat, !(SNP %in% miss.snps$SNP))
        if(remove_tmps) file.remove(miss.file)
        #### Recursion ####
        priors <- POLYFUN_import_priors(
            locus_dir = locus_dir,
            dat = filt_DT,
            force_new_priors = force_new_priors,
            conda_env = conda_env,
            verbose = verbose)
        return(priors)
    }

    ## [4.] Verify output was created
    if(!file.exists(snp_w_priors.file)){
        stop("PolyFun extract_snpvar.py did not produce output file: ",
             snp_w_priors.file)
    }

    ## [5.] Import new priors
    priors <- POLYFUN_read_priors(snp_w_priors.file = snp_w_priors.file,
                                   nThread = nThread,
                                   verbose = verbose)
    if(remove_tmps) file.remove(snp.path)
    return(priors)
}
