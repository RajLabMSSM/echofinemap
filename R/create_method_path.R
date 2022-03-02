#' Create method path
#' 
#' Create a fine-mapping method-specific path.
#' @family finemapping functions
#' @export
create_method_path <- function(locus_dir,
                               finemap_method,
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
                               paste0("*Multi-finemap.tsv",
                                      if(compress) ".gz" else "*"))
    } else{
        file_path <- file.path(method_dir,
                               paste0(
                                   paste(paste0(LD_reference,"_LD"),
                                         finemap_method,"tsv", sep="."),
                                      if(compress) ".gz" else "*")
        )
    }
    return(file_path)
}
