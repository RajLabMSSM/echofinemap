#' POLYFUN help
#' 
#' Display PolyFun help. 
#' Will automatically install PolyFun if has not been installed already.
#' @param polyfun [Optional] Path to PolyFun executable.
#' @inheritParams echoconda::find_python_path
#' @export
#' @family polyfun
#' @returns PolyFun help message as character vector.
#' @examples 
#' echofinemap::POLYFUN_help()
POLYFUN_help <- function(polyfun=NULL,
                         conda_env="echoR_mini"){
    POLYFUN_install()
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    python <- echoconda::find_python_path(conda_env = conda_env)
    messager("Retrieving PolyFun --help documentation.")
    cmd <- paste(python,
                 file.path(polyfun,"polyfun.py"),
                 "--help")
    out <- system(cmd, intern = TRUE)
    message(paste(out,collapse = "\n"))
    return(out)
}
