POLYFUN_sample_size_arg <- function(fullSS_path,
                                    sample_size,
                                    nrows=1000,
                                    verbose=TRUE){
    
    header <- echodata::get_header(path = fullSS_path, 
                                   nrows = nrows, 
                                   verbose = verbose)
    if((any(c("N_cases") %in% header) &
        any(c("N_controls") %in% header)) |
       "N" %in% header){
        if(!is.null(sample_size)){
            wrn <- paste(
                "Cannot both specify sample_size (--n) and have an",
                "N_cases/N_controls column in the sumstats file.",
                "Using N_cases/N_controls columns instead.") 
            warning(wrn) 
        } else {
            message("Using N_cases/N_controls columns.")
        } 
        sample_size_arg <- NULL
    } else {
        #### If N_cases/N_controls aren't available and sample size is NULL #### 
        if(is.null(sample_size)){
            messager(
                "sample_size not provided.",
                "Inferring from taking max(N) from the top",
                formatC(nrows,big.mark = ","),"rows of fullSS",
                v=verbose)
            dat <- data.table::fread(fullSS_path, nrows = nrows)
            sample_size <- echodata::get_sample_size(dat = dat, 
                                                     return_only = max, 
                                                     na.rm=TRUE) 
        } 
        if(is.null(sample_size)){
            stop("Could not infer sample_size from data.",
                 "Please supply directly to sample_size argument.")
        }
        else {
            sample_size_arg <- paste("--n",sample_size)
        }
    } 
    return(sample_size_arg)
}