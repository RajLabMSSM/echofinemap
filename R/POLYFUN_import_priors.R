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
POLYFUN_import_priors <- function(locus_dir,
                                  dat=NULL,
                                  polyfun=NULL, 
                                  force_new_priors=TRUE,
                                  remove_tmps=FALSE,
                                  nThread=1,
                                  conda_env="echoR",
                                  verbose=TRUE){
    python <- echoconda::find_python_path(conda_env = conda_env)
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    dataset <- basename(dirname(locus_dir))
    locus <- basename(locus_dir)
    PF.output.path <- file.path(locus_dir, "PolyFun")
    snp_w_priors.file <- file.path(PF.output.path,
                                   "snps_with_priors.snpvar.tsv.gz")
    
    if((file.exists(snp_w_priors.file)) & force_new_priors==FALSE){ 
        #### Import existing priors ####
        priors <- POLYFUN_read_priors(snp_w_priors.file = snp_w_priors.file,
                                      nThread = nThread, 
                                      verbose = verbose)
        return(priors)
    } else {
        if(is.null(dat)) stop("dat is a required argument in this condition.")
        dat <- POLYFUN_initialize(dat=dat,
                                  locus_dir=locus_dir,
                                  verbose = verbose)
        chrom <- dat$CHR[1]
        # [1]. Prepare input
        snp.path <- POLYFUN_prepare_snp_input(PF.output.path=PF.output.path,
                                              locus_dir=locus_dir,
                                              dat=dat)
        # [2.] Retrieve priors 
        try({
            cmd <- paste(python,
                         file.path(polyfun,"extract_snpvar.py"),
                         ## arg name was changed from --snps ==> --sumstats
                         ## at some point.
                         # "--snps",snp.path,
                         "--sumstats",snp.path,
                         "--out",snp_w_priors.file)
            cmd_print(cmd, v=verbose)
            system(cmd)
            messager("++ Remove tmp file.")
        })
        
        miss.file <- paste0(snp_w_priors.file,".miss.gz")
        if(file.exists(miss.file)){
            messager("+ PolyFun:: Rerunning after removing missing SNPs.")
            miss.snps <- data.table::fread(miss.file,
                                           nThread = nThread)
            filt_DT <- subset(dat, !(SNP %in% miss.snps$SNP) )
            if(remove_tmps){file.remove(miss.file)}
            #### Recursion ####
            priors <- POLYFUN_import_priors(
                locus_dir=locus_dir,
                dat=filt_DT,
                force_new_priors=force_new_priors,
                conda_env = conda_env,
                verbose = verbose)
        }
        #### Import new priors ####
        priors <- POLYFUN_read_priors(snp_w_priors.file = snp_w_priors.file,
                                       nThread = nThread, 
                                       verbose = verbose)
        return(priors)
    }
    if(remove_tmps){ file.remove(snp.path) }
}
