PAINTOR_is_installed <- function(paintor_path,
                                 force_reinstall){
    
    paintor_ex <- PAINTOR_find_executable(paintor_path = paintor_path)
    install_log <- file.path(paintor_path,"install_log.txt") 
    is_installed <- file.exists(install_log) && 
        file.exists(paintor_ex) &&
        isFALSE(force_reinstall) 
    if(is_installed){
        h <- PAINTOR_help(verbose = FALSE)
        is_installed <- any(grepl("Welcome to PAINTOR",h))
    }
    return(is_installed)
}