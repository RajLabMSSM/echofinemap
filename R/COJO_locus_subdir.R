#' Make \emph{GCTA-COJO} path
#' 
#' @family COJO
#' @keywords internal
#' @source
#' \url{https://www.nature.com/articles/ng.2213}
#' \url{https://www.cell.com/ajhg/fulltext/S0002-9297(10)00598-7}
#' \url{https://cnsgenomics.com/software/gcta/#Overview}
#' @source
#' \code{
#' locus_dir <- echodata::locus_dir
#' cojo_dir <- COJO.make_locus_subdir(locus_dir=locus_dir)
#' }
COJO_locus_subdir <- function(locus_dir){
    cojo_dir <- file.path(locus_dir, "COJO")
    dir.create(cojo_dir, recursive = TRUE, showWarnings  = FALSE)
    return(cojo_dir)
}
