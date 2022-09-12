FINEMAP_check_version <- function(FINEMAP_path=
                                      FINEMAP_find_executable(verbose = FALSE),
                                  verbose=TRUE){
    #### FINEMAP does not have a -v or --version flag. ####
    out <- system(paste(FINEMAP_path,"-h"), intern = TRUE)
    out_split <- strsplit(grep("Welcome to FINEMAP",out,
                               value = TRUE)[1]," ")[[1]]
    finemap_version <- gsub("v","",out_split[grepl("v",out_split)])
    messager("Inferred FINEMAP version:",
             finemap_version,v=verbose)
    return(package_version(finemap_version))
}


