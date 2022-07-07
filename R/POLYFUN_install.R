#' Install PolyFun
#' 
#' Install PolyFun (if not already installed) from the GitHub submodule.
#' @export
#' @param force_new Force a new installation 
#' even when PolyFun is already installed. 
#' @returns Null.
POLYFUN_install <- function(force_new=FALSE){
   POLYFUN_installed <- function(){
       dirs <- grep("polyfun", 
                    list.dirs(system.file("tools",package = "echofinemap"),
                              recursive = FALSE), value = TRUE) 
       "polyfun" %in% basename(dirs)
   }
   is_installed <- POLYFUN_installed()
    if((isFALSE(is_installed)) | isTRUE(force_new) ){
        install <- readline(
            prompt = paste("PolyFun is not installed.",
                           "Reinstall echofinemap with PolyFun?",
                           "This will automatically restart R. (y/n): ")
        )
        if(tolower(install)=="y"){
            messager("Installing PolyFun submodule.")
            install_submodule_git("https://github.com/RajLabMSSM/echofinemap")
        }
        is_installed <- POLYFUN_installed()
        if(isFALSE(is_installed)) stop("PolyFun is not installed.")
    } else {
        messager("PolyFun submodule already installed.")
    }
}
