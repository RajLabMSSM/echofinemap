



#' Prepare SNP input for PolyFun
#'
#' PolyFun requires a space-delimited (gzipped or not) file with these columns:
#' \itemize{
#' \item{CHR}
#' \item{BP}
#' \item{A1}
#' \item{A2}
#' }
#' @keywords internal
#' @family polyfun
#' @examples
#' BST1 <- echodata::BST1; locus_dir <- echodata::locus_dir;
#' dat <- BST1
#' PF.output.path <- file.path(locus_dir, "PolyFun")
#' POLYFUN_prepare_snp_input(PF.output.path=PF.output.path, locus_dir=locus_dir, dat=dat)
POLYFUN_prepare_snp_input <- function(PF.output.path,
                                      locus_dir,
                                      dat=NULL,
                                      nThread=1){
    messager("PolyFun:: Preparing SNP input file...")
    if(!"A1" %in% colnames(dat)) A1 <- NULL;
    if(!"A2" %in% colnames(dat)) A2 <- NULL;
    PF.dat <- dplyr::select(dat,
                            SNP,
                            CHR,
                            BP=POS,
                            A1=A1,
                            A2=A2)
    messager("+ PolyFun::",nrow(PF.dat),"SNPs identified.")
    snp.path <- file.path(PF.output.path,"snps_to_finemap.txt.gz")
    messager("+ PolyFun:: Writing SNP file ==>",snp.path)
    dir.create(dirname(snp.path), recursive = TRUE, showWarnings  = FALSE)
    data.table::fwrite(PF.dat, file = snp.path,
                       nThread = nThread, sep = " ")
    # Check if it's actually gzipped
    ## (in older versions of data.table, files with the .gz extension are automatically gzipped)
    if(!R.utils::isGzipped(snp.path)){
        R.utils::gzip(filename=snp.path, destname=snp.path, overwrite=TRUE)
    }
    return(snp.path)
}
