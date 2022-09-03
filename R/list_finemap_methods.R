#' List fine-mapping methods 
#' 
#' List all fine-mapping methods currently available in \pkg{echofinemap}.
#' @inheritParams multifinemap
#' @export
#' @examples 
#' finemap_methods <- echofinemap::list_finemap_methods()
lfm <- list_finemap_methods <- function(finemap_methods = NULL,
                                        verbose = TRUE){
    d <- required_cols() 
    if(is.null(finemap_methods)) finemap_methods <- d$method 
    skip_methods <- finemap_methods[!finemap_methods %in% d$method]
    finemap_methods <- finemap_methods[finemap_methods %in% d$method]
    if(length(skip_methods)>0) {
        messager(
            "Warning: Some fine-mapping methods not recognized",
            "and will be omitted:\n",
                paste("-",skip_methods,collapse = "\n"),
                verbose=TRUE)
        messager("See echofinemap::list_finemap_methods()",
                 "for available methods.",v=verbose)
    }
    return(finemap_methods)
}
