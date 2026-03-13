#' Install PAINTOR via command line
#'
#' Compile PAINTOR from source using the bundled install script.
#' @keywords internal
#' @source
#' \href{https://github.com/gkichaev/PAINTOR_V3.0/issues/49}{
#' Installation issues with gcc versions}
PAINTOR_install <- function(paintor_path = NULL,
                            force_reinstall = FALSE,
                            verbose = TRUE){
    paintor_path <- PAINTOR_find_folder(paintor_path = paintor_path)
    if(!nzchar(paintor_path) || !dir.exists(paintor_path)){
        stop("PAINTOR source not found. The PAINTOR_V3.0 git submodule ",
             "is required to compile PAINTOR.\n",
             "Reinstall echofinemap with submodules:\n",
             "  remotes::install_github('RajLabMSSM/echofinemap')")
    }
    if(PAINTOR_is_installed(paintor_path = paintor_path,
                            force_reinstall = force_reinstall)){
        messager("PAINTOR:: PAINTOR_V3.0 already installed.", v = verbose)
    } else {
        install_script <- file.path(paintor_path, "install.sh")
        if(!file.exists(install_script)){
            stop("PAINTOR install.sh not found at: ", paintor_path, "\n",
                 "The git submodule may be empty. Reinstall echofinemap:\n",
                 "  remotes::install_github('RajLabMSSM/echofinemap')")
        }
        cmd <- paste("cd", shQuote(paintor_path), "&& bash install.sh")
        messager("PAINTOR:: Installing PAINTOR_V3.0", v = verbose)
        out <- tryCatch({
            system(cmd, intern = TRUE)
        }, error = function(e){
            messager("PAINTOR:: Installation failed:", e$message,
                     v = verbose)
            NULL
        })
        install_log <- file.path(paintor_path, "install_log.txt")
        messager("PAINTOR:: Writing log:", install_log, v = verbose)
        data.table::fwrite(list(out), install_log)
    }
    return(paintor_path)
}
