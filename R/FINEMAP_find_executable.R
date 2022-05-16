#' Find FINEMAP executable 
#' 
#' Retrieve location of \code{FINEMAP} executable.
#' @family FINEMAP
#' @importFrom tools R_user_dir
#' @importFrom utils untar
#' @keywords internal
#' @source
#' \url{http://www.christianbenner.com}
#' @examples
#' FINEMAP_path <- echofinemap:::FINEMAP_find_executable(version="1.3.1")
FINEMAP_find_executable <- function(FINEMAP_path=NULL,
                                    OS=NULL,
                                    save_dir = file.path(
                                        tools::R_user_dir(
                                            package = "echofinemap",
                                            which = "cache"),
                                        "FINEMAP"
                                    ),
                                    version=package_version("1.4.1"),
                                    verbose=TRUE){
    if(is.null(OS)) OS <- get_os()
    # messager("+ Using FINEMAP",paste0("v",version),v=verbose)
    if(OS=="osx"){
        exec <- paste0("finemap_v",version,"_MacOSX")
    } else{
        exec <- paste0("finemap_v",version,"_x86_64")
    }   
    #### (Download and) test executable ###
    if(is.null(FINEMAP_path)) {
        FINEMAP_path <- file.path(save_dir,exec,exec) 
    }
    if(!file.exists(FINEMAP_path)){ 
        tgz <- get_data(fname = paste0(exec,".tgz"), 
                        storage_dir = save_dir) 
        utils::untar(tarfile = tgz,
                     exdir = dirname(tgz)) 
    }
    return(FINEMAP_path)
}
