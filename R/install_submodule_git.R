#' Install a submodule 
#' 
#' Install a submodule from a remote GitHub repository.
#' @keywords internal
#' @source 
#' \href{https://github.com/r-lib/devtools/pull/751#issuecomment-215540807}{
#' GitHub Issue}
install_submodule_git <- function(x, ...) {
    requireNamespace("devtools")
    install_dir <- tempfile()
    system(paste("git clone --recursive", shQuote(x), shQuote(install_dir)))
    devtools::install(install_dir, ...)
}
