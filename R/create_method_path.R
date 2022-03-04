#' Create method path
#' 
#' Create a fine-mapping method-specific path.
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
                               create_dir=TRUE,
                               compress=FALSE){
    method_dir <- file.path(locus_dir, finemap_method)
    # Make finemapping results folder
    if(create_dir) dir.create(method_dir, 
                              recursive = TRUE, showWarnings  = FALSE)
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
                                   paste(paste0(LD_reference,"_LD"),
                                         finemap_method,"tsv", sep="."),
                                      if(compress) ".gz" else "*")
        )
    }
    if(!include_astrices) file_path <- gsub("\\*","",file_path)
    return(file_path)
}
