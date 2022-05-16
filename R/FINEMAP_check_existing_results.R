FINEMAP_check_existing_results <- function(dat,
                                           locus_dir,
                                           credset_thresh,
                                           finemap_version,
                                           force_new = FALSE,
                                           verbose = TRUE){
    
    #### Delete old data ####
    ## Avoids reading in files from old runs by accident.
    finemap_dir <- file.path(locus_dir,"FINEMAP")
    if(force_new){
        if(file.exists(finemap_dir)) {
            unlink(finemap_dir, recursive = TRUE, force = TRUE)
        }
    }
    dir.create(finemap_dir, showWarnings = FALSE, recursive = TRUE)
    file_options <- FINEMAP_check_files(locus_dir = locus_dir) 
    if((length(file_options)>0) &&
       isFALSE(force_new)){
        messager("Importing pre-computed FINEMAP results files.",
                 "Set force_new=TRUE to compute new results.",v=verbose)
        ## Use tryCatch because sometimes the requested version of FINEMAP
        ## if different from the version that produced the results.
        dat <- tryCatch({
            FINEMAP_process_results(dat = dat,
                                    locus_dir = locus_dir,
                                    credset_thresh = credset_thresh, 
                                    finemap_version = finemap_version,
                                    verbose = verbose)
        }, error=function(e){message(e);NULL})
        return(dat)
    } else {return(NULL)}
}
