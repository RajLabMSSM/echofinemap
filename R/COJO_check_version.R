COJO_check_version <- function(gcta_path=NULL,
                               verbose=TRUE){
    gcta_path <- echoconda::find_executables_remote(path = gcta_path,
                                                    tool = "gcta",
                                                    verbose = verbose)[[1]]
    out <- suppressWarnings(system(paste(gcta_path), intern = TRUE))
    l  <- grep(" version ",out, value = TRUE)
    split <- strsplit(l," ")[[1]]
    v <- package_version(split[which(split=="version")+1])
    messager("GCTA-COJO version:",v,v=verbose)
    return(v)
}