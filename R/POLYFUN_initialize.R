#' PolyFun: Initialize 
#' 
#' Create output dir and import SNP data.frame.
#' @keywords internal
#' @family polyfun
#' @source 
#' \code{
#' BST1 <- echodata::BST1; locus_dir <- echodata::locus_dir;
#' dat <- POLYFUN_initialize(locus_dir=locus_dir, dat=BST1)
#' }
POLYFUN_initialize <- function(locus_dir,
                               dat=NULL,
                               nThread=1,
                               verbose=TRUE){
    dataset <- basename(dirname(locus_dir))
    locus <- basename(locus_dir)
    # Create path
    PF.output.path <- file.path(locus_dir, "PolyFun")
    dir.create(PF.output.path, showWarnings = FALSE, recursive = TRUE)
    # Import SNPs
    if(is.null(dat)){
        if(is.null(locus)){
            messager(
                "PolyFun:: Importing summary stats from disk: genome-wide",
                v=verbose)
            dat <- data.table::fread(Directory_info(dataset, "fullSS.local"),
                                     nThread = nThread)
        }
        messager("PolyFun:: Importing summary stats from disk:",locus,
                 v=verbose)
        dat <- data.table::fread(
            file.path(dirname(Directory_info(dataset, "fullSS.local")),locus,
                      paste(locus,dataset,"subset.tsv.gz",sep="_")), 
            nThread = nThread)
    }
    return(dat)
}
