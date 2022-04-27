#' PolyFun: Munge summary stats
#' 
#' Munge summary statistics using the PolyFun implementation of the LDSSC
#'  munge sum stats python script (\code{munge_polyfun_sumstats.py}).
#' @keywords internal
#' @family polyfun
#' @examples
#' \dontrun{
#' fullSS_path <- echodata::example_fullSS()
#' #### Remove duplicates ####
#' dat <- data.table::fread(fullSS_path)
#' munged_path <- POLYFUN_munge_summ_stats(fullSS_path=fullSS_path)
#' }
POLYFUN_munge_summ_stats <- function(polyfun=NULL,
                                     fullSS_path,
                                     locus_dir=tempdir(),
                                     sample_size=NULL,
                                     min_INFO=0,
                                     min_MAF=0.001,
                                     chi2_cutoff=30,
                                     keep_hla=FALSE,
                                     no_neff=FALSE,
                                     force_new_munge=FALSE,
                                     conda_env="echoR_mini",
                                     verbose=TRUE){
    
    python <- echoconda::find_python_path(conda_env = conda_env)
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    PF.output.path <- file.path(locus_dir, "PolyFun")
    dir.create(PF.output.path, showWarnings = FALSE, recursive = TRUE)
    munged_path <- file.path(
        PF.output.path,
        paste0(gsub("\\.gz|\\.txt|\\.tsv|\\.csv","",
                    basename(fullSS_path)),".munged.parquet"))
    
    # PolyFun requires space-delimited file with the following columns 
    #(munging can recognize several variations of these names):
    ## SNP CHR BP ....and....
    ## either a p-value, an effect size estimate and its standard error,
    #a Z-score or a p-value 
    sample_size_arg <- POLYFUN_sample_size_arg(
        fullSS_path=fullSS_path,
        sample_size=sample_size, 
        verbose=verbose)
    ##### Run munge_polyfun_sumstats.py ####
    if(!file.exists(munged_path) | force_new_munge){
        messager("+ PolyFun:: Initiating data munging pipeline...",v=verbose)
        cmd <- paste(python,
                     file.path(polyfun,"munge_polyfun_sumstats.py"),
                     "--sumstats",fullSS_path,
                     sample_size_arg, # Study sample size
                     "--out",munged_path,
                     "--min-info",min_INFO,
                     "--min-maf",min_MAF
                     # "--chi2-cutoff",chi2_cutoff,
                     # "--keep-hla",if(keep_hla)"True"else"False",
                     # "--no-neff",if(no_neff)"True"else"False"
        )
        echoconda::cmd_print(cmd, verbose=verbose)
        system(cmd)
    } else {
        messager("+ PolyFun:: Existing munged summary stats files detected.",
                 v=verbose)
    }
    return(munged_path)
}

