#' POLYFUN help
#' 
#' Display PolyFun help.
#' @keywords internal
#' @family polyfun
#' 
#' @source
#' \code{
#' echofinemap:::POLYFUN_help()
#' }
POLYFUN_help <- function(polyfun=NULL,
                         conda_env="echoR_mini"){
    polyfun <- POLYFUN_find_polyfun_folder(polyfun_path = polyfun)
    python <- echoconda::find_python_path(conda_env = conda_env)
    cmd <- paste(python,
                 file.path(polyfun,"polyfun.py"),
                 "--help")
    system(cmd)
}
