#' PolyFun: Initialize 
#' 
#' Create output dir.
#' @keywords internal
#' @family polyfun
#' @source 
#' \code{
#' locus_dir <- echodata::locus_dir;
#' dat <- POLYFUN_initialize(locus_dir=locus_dir)
#' }
POLYFUN_initialize <- function(locus_dir){
    PF.output.path <- file.path(locus_dir, "PolyFun")
    dir.create(PF.output.path, showWarnings = FALSE, recursive = TRUE) 
    return(PF.output.path)
}
