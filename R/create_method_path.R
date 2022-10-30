#' Create method path
#' 
#' Create a fine-mapping method-specific path.
#' @param finemap_method Fine-mapping method to run. 
#' See \link[echofinemap]{lfm} for a list of all fine-mapping methods currently
#' available. 
#' @param include_astrices Whether to keep any astrices in file path.
#' @param compress Whether to add ".gz" at the end of the file path. 
#' @inheritParams multifinemap
#' @family finemapping functions
#' @export
#' @examples 
#' locus_dir <- echodata::locus_dir
#' path <- echofinemap::create_method_path(locus_dir = locus_dir, 
#'                                         finemap_method = "SUSIE")
create_method_path <- function(locus_dir,
                               finemap_method,
                               include_astrices=FALSE,
                               LD_reference=NULL, 
                               compress=FALSE){
    
    method_dir <- file.path(locus_dir, finemap_method) 
    # Return results file name
    # dataset <- basename(dirname(locus_dir))
    # locus <- basename(locus_dir)
    if(is.null(LD_reference)){
        file_path <- file.path(method_dir, 
                               paste0(paste0("*",finemap_method,".tsv"),
                                      if(compress) ".gz" else "*"))
    } else{
        file_path <- file.path(method_dir,
                               paste0(
                                   paste(paste0(basename(LD_reference),"_LD"),
                                         finemap_method,"tsv", sep="."),
                                      if(compress) ".gz" else "*")
        )
    }
    # Make finemapping results folder 
    dir.create(dirname(file_path),  recursive = TRUE, showWarnings  = FALSE) 
    if(isFALSE(include_astrices)) file_path <- gsub("\\*","",file_path)
    return(file_path)
}
