#' @family finemapping functions
#' @keywords internal
save_finemap_results <- function(dat,
                                 path,
                                 nThread=1,
                                 verbose=TRUE){
    messager("Saving merged finemapping results ==>",path,v=verbose)
    data.table::fwrite(x = dat, 
                       file = path, 
                       sep = "\t", 
                       na = NA, quote = FALSE,
                       nThread = nThread)
}

