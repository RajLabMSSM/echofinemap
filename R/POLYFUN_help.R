#' POLYFUN help
#' 
#' Display PolyFun help. 
#' Will automatically install PolyFun if has not been installed already.
#' @param polyfun [Optional] Path to PolyFun executable.
#' @inheritParams echoconda::find_python_path
#' @family polyfun
#' @returns PolyFun help message as character vector.
#' 
#' @export
#' @importFrom echoconda find_python_path
#' @examples 
#' h <- echofinemap::POLYFUN_help()
POLYFUN_help <- function(polyfun=NULL,
                         conda_env="echoR_mini",
                         verbose=TRUE){
    POLYFUN_install(verbose = verbose)
    polyfun <- POLYFUN_find_folder(polyfun_path = polyfun)
    python <- echoconda::find_python_path(conda_env = conda_env)
    messager("Retrieving PolyFun --help documentation.",v=verbose)
    cmd <- paste(python,
                 file.path(polyfun,"polyfun.py"),
                 "--help")
    out <- system(cmd, intern = TRUE)
    messager(paste(out,collapse = "\n"), v=verbose)
    return(out)
}
