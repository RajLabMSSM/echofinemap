PAINTOR_check_version <- function(paintor_path=NULL,
                                  verbose=FALSE){
    h <- PAINTOR_help(paintor_path = paintor_path,
                      verbose = verbose)
    l <- grep("Welcome to PAINTOR",h,value = TRUE)
    split <- strsplit(l," ")[[1]]
    v <- package_version(gsub("\\.$|^v","",split[which(split=="PAINTOR")+1]))
    messager("PAINTOR version:",v,v=verbose)
    return(v)
}
