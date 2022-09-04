PAINTOR_help <- function(paintor_path=NULL,
                         verbose = TRUE){
    paintor_path <- PAINTOR_find_folder(paintor_path=paintor_path)
    out <- system(file.path(paintor_path,"PAINTOR"),intern = TRUE)
    if(verbose) cat(paste(out,collapse ="\n"))
    return(out)
}
