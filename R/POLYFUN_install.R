#' Install PolyFun
#'
#' Check if PolyFun is installed from the GitHub submodule.
#' If not installed, provides instructions rather than using
#' interactive prompts that block non-interactive sessions.
#' @export
#' @param force_new Force a new installation
#' even when PolyFun is already installed.
#' @param verbose Print messages.
#' @returns Logical indicating whether PolyFun is installed.
POLYFUN_install <- function(force_new=FALSE,
                            verbose=TRUE){
    polyfun_dir <- POLYFUN_find_folder()
    is_installed <- dir.exists(polyfun_dir) &&
        file.exists(file.path(polyfun_dir, "extract_snpvar.py"))

    if(isTRUE(is_installed) && isFALSE(force_new)){
        messager("PolyFun submodule already installed.", v = verbose)
        return(invisible(TRUE))
    }

    if(interactive()){
        install <- readline(
            prompt = paste("PolyFun is not installed.",
                           "Reinstall echofinemap with PolyFun?",
                           "This will automatically restart R. (y/n): ")
        )
        if(tolower(install) == "y"){
            messager("Installing PolyFun submodule.", v = verbose)
            install_submodule_git(
                "https://github.com/RajLabMSSM/echofinemap"
            )
        }
    }

    ## Re-check
    is_installed <- dir.exists(polyfun_dir) &&
        file.exists(file.path(polyfun_dir, "extract_snpvar.py"))
    if(isFALSE(is_installed)){
        stop("PolyFun is not installed. Reinstall echofinemap with:\n",
             "  remotes::install_github('RajLabMSSM/echofinemap')")
    }
    return(invisible(TRUE))
}
