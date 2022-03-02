#' Munge summary stats
#' @keywords internal
#' @family polyfun
#' @examples
#' \dontrun{
#' data("genome_wide_dir");
#' fullSS_path <- example_fullSS(fullSS_path="~/Desktop/Nalls23andMe_2019.fullSS_subset.tsv")
#' munged_path <- POLYFUN_munge_summ_stats(fullSS_path=fullSS_path, locus_dir=genome_wide_dir, force_new_munge=TRUE)
#' }
POLYFUN_munge_summ_stats <- function(polyfun=NULL,
                                     fullSS_path,
                                     locus_dir,
                                     sample_size=NULL,
                                     min_INFO=0,
                                     min_MAF=0.001,
                                     force_new_munge=FALSE,
                                     conda_env="echoR"){
    python <- echoconda::find_python_path(conda_env = conda_env)
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    PF.output.path <- file.path(locus_dir, "PolyFun")
    dir.create(PF.output.path, showWarnings = FALSE, recursive = TRUE)
    munged_path <- file.path(PF.output.path,
                             paste0(gsub("\\.gz|\\.txt|\\.tsv|\\.csv","",basename(fullSS_path)),".munged.parquet"))
    # PolyFun requires space-delimited file with the following columns (munging can recognize several variations of these names):
    ## SNP CHR BP ....and....
    ## either a p-value, an effect size estimate and its standard error, a Z-score or a p-value
    
    header <- echodata::get_header(large_file = fullSS_path)
    if("N_cases" %in% header & "N_controls" %in% header){
        warning("Cannot both specify sample_size (--n) and have an N_cases/N_controls column in the sumstats file. Using N_cases/N_controls columns instead.")
        sample_size_arg <- NULL
    } else {sample_size_arg <- paste("--n",sample_size)}
    
    if(!file.exists(munged_path) | force_new_munge){
        messager("+ PolyFun:: Initiating data munging pipeline...")
        cmd <- paste(python,
                     file.path(polyfun,"munge_polyfun_sumstats.py"),
                     "--sumstats",fullSS_path,#Directory_info(dataset_name = dataset, ifelse(server,"fullSS",fullSS.loc)),
                     sample_size_arg, # Study sample size
                     "--out",munged_path,
                     "--min-info",min_INFO,
                     "--min-maf",min_MAF
        )
        print(cmd)
        system(cmd)
    } else {messager("+ PolyFun:: Existing munged summary stats files detected.")}
    return(munged_path)
}

