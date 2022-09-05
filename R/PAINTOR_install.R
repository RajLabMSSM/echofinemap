#' Install PAINTOR via command line
#'
#' Currently there is no R or conda distribution of PAINTOR_
#' @keywords internal
#' @source
#' \href{https://github.com/gkichaev/PAINTOR_V3.0/issues/49}{
#' Installation issues with gcc versions}
#' \href{https://formulae.brew.sh/formula/gcc}{
#' Only certain subversions of gcc are available via brew}
PAINTOR_install <- function(paintor_path = NULL,
                            force_reinstall = FALSE,
                            verbose = TRUE){
    paintor_path <- PAINTOR_find_folder(paintor_path = paintor_path) 
    install_log <- file.path(paintor_path,"install_log.txt") 
    if(PAINTOR_is_installed(paintor_path = paintor_path,
                            force_reinstall = force_reinstall)){ 
        messager("PAINTOR:: PAINTOR_V3.0 already installed.",v=verbose)
    }else {
        cmd <- paste("cd", paintor_path,"&& bash install.sh")
        messager("PAINTOR:: Installing PAINTOR_V3.0")
        out <- tryCatch(expr = {
            null <- setup_gcc(version = "clang")
            system(cmd, intern = TRUE)
        }, error= function(e){message(e);NULL})
        messager("PAINTOR:: Writing log:",install_log)
        data.table::fwrite(list(out), install_log)
    }
    return(paintor_path)
}
