PAINTOR_find_executable <- function(paintor_path=NULL){
    paintor_path <- PAINTOR_find_folder(paintor_path = paintor_path)
    paintor_ex <- file.path(paintor_path,"PAINTOR")
    if(!file.exists(paintor_ex)) stop("PAINTOR executable cannot be found.")
    return(paintor_ex)
}