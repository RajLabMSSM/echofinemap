#' Find FINEMAP executable
#'
#' Retrieve location of \code{FINEMAP} executable.
#' First checks if \code{finemap} is already available on the system PATH
#' (e.g. installed via \code{conda install -c bioconda finemap}),
#' then falls back to downloading the binary from GitHub releases.
#' On Apple Silicon Macs, automatically sets up x86_64 dynamic libraries
#' via \code{\link{FINEMAP_setup_dylibs}}.
#' @family FINEMAP
#' @importFrom tools R_user_dir
#' @importFrom utils untar
#' @importFrom echodata get_os
#' @keywords internal
#' @source
#' \href{http://www.christianbenner.com}{FINEMAP site}
#' \href{https://support.bioconductor.org/p/9141978/}{Similar dynlib error in R}
#' @examples
#' \dontrun{
#' FINEMAP_path <- echofinemap:::FINEMAP_find_executable(version="1.3.1")
#' }
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

    if(is.null(OS)) OS <- echodata::get_os()

    #### Check for user-supplied path ####
    if(!is.null(FINEMAP_path) && file.exists(FINEMAP_path)){
        Sys.chmod(FINEMAP_path, "0755")
        return(FINEMAP_path)
    }

    #### Check system PATH first ####
    ## e.g. conda install -c bioconda finemap
    system_finemap <- Sys.which("finemap")
    if(nchar(system_finemap) > 0){
        messager("Found FINEMAP on system PATH:",
                 system_finemap, v = verbose)
        return(unname(system_finemap))
    }

    #### Download from GitHub releases ####
    if(OS=="osx"){
        exec <- paste0("finemap_v",version,"_MacOSX")
    } else{
        exec <- paste0("finemap_v",version,"_x86_64")
    }
    if(is.null(FINEMAP_path)) {
        FINEMAP_path <- file.path(save_dir,exec,exec)
    }
    if(!file.exists(FINEMAP_path)){
        messager("Downloading FINEMAP v", version, "...", v = verbose)
        tgz <- get_data(fname = paste0(exec,".tgz"),
                        save_dir = save_dir)
        utils::untar(tarfile = tgz,
                     exdir = dirname(tgz))
    }
    Sys.chmod(FINEMAP_path, "0755")
    #### Setup dylibs on Apple Silicon ####
    FINEMAP_setup_dylibs(FINEMAP_path = FINEMAP_path,
                         verbose = verbose)
    return(FINEMAP_path)
}
