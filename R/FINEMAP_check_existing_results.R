FINEMAP_check_existing_results <- function(locus_dir,
                                           credset_thresh,
                                           finemap_version,
                                           master_path,
                                           force_new = FALSE,
                                           verbose = TRUE){
    if(any(file.exists(file.path(dirname(master_path),
                                 c("data.cred","data.snp","data.config")))) &&
       force_new==FALSE){
        messager("+ FINEMAP:: Importing pre-computed FINEMAP results files.",
                 "Set force_new=TRUE to compute new results.",v=verbose)
        dat <- FINEMAP_process_results(locus_dir = locus_dir,
                                       dat = dat,
                                       credset_thresh = credset_thresh,
                                       results_file = ".cred",
                                       finemap_version = finemap_version)
        return(dat)
    } else {return(NULL)}
}