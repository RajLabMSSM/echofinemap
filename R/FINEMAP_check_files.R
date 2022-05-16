FINEMAP_check_files <- function(locus_dir,
                                file_options=c(".cred",".snp",".config")){
    ### In FINEMAP v1.3, only one .cred file are produced.
    ### In FINEMAP v1.4, multiple FINEMAP files with # suffixes are produced. 
    if(is.null(file_options)) file_options <- c(".cred",".snp",".config")
    .cred_files <- list.files(path = file.path(locus_dir,"FINEMAP"),
                              pattern = "data.cred*", 
                              full.names = TRUE)
    .cred_exists <- any(exists_not_empty(.cred_files))
    .snp_exists <- any(exists_not_empty(file.path(locus_dir,
                                                  "FINEMAP/data.snp")) )
    .config_exists <- any(exists_not_empty(file.path(locus_dir,
                                                     "FINEMAP/data.config")))
    file_options <- file_options[
        c(.cred_exists,.snp_exists,.config_exists)
    ]
    return(file_options)
}
