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
                                    version="1.4",
                                    verbose=TRUE){
    if(is.null(OS)){OS <- get_os()}
    
    if(version=="1.4"){
        messager("+ Using FINEMAP v1.4",v=verbose)
        if(OS=="osx"){
            exec <- "finemap_v1.4_MacOSX"
        } else{
            exec <- "finemap_v1.4_x86_64"
        }
    }
    if(version=="1.3.1") {
        messager("+ Using FINEMAP v1.3.1",v=verbose)
        if(OS=="osx"){
            exec <- "finemap_v1.3.1_MacOSX"
        } else{
            exec <- "finemap_v1.3.1_x86_64"
        }
    }
    if(version=="1.3") {
        messager("+ Using FINEMAP v1.3",v=verbose)
        if(OS=="osx"){
            exec <- "finemap_v1.3_MacOSX"
        } else{
            exec <- "finemap_v1.3_x86_64"
        }
    }
    #### (Download and) test executable ###
    if(is.null(FINEMAP_path)) FINEMAP_path <- file.path(save_dir,exec,exec) 
    if(!file.exists(FINEMAP_path)){
        dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
        tgz <- get_data(fname = paste0(exec,".tgz"), 
                        storage_dir = save_dir) 
        utils::untar(tarfile = tgz,
                     exdir = dirname(tgz)) 
    }  
    version <- FINEMAP_check_version(FINEMAP_path=FINEMAP_path, 
                                     verbose=TRUE) 
    return(FINEMAP_path)
}
