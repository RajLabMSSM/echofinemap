FINEMAP_check_files <- function(locus_dir,
                                results_file){
    ### In FINEMAP v1.3, only one .cred file are produced.
    ### In FINEMAP v1.4, multiple FINEMAP files with # suffixes are produced.
    .cred_files <- list.files(file.path(locus_dir,"FINEMAP"), "data.cred", 
                              full.names = TRUE)
    .cred_exists <- length(.cred_files)>0
    .snp_exists <- file.exists(file.path(locus_dir,"FINEMAP/data.snp"))
    .config_exists <- file.exists(file.path(locus_dir,"FINEMAP/data.config"))
    file_options <- c(".cred",".snp",".config")[
        c(.cred_exists,.snp_exists,.config_exists)
    ]
    if(!results_file %in% file_options){
        messager(results_file,"not detected.",
                 "Using",file_options[1],"instead.")
        return(file_options[1])
    }else { return(results_file)}
}
