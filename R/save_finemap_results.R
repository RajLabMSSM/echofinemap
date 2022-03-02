#' @family finemapping functions
#' @keywords internal
save_finemap_results <- function(dat,
                                 file_dir,
                                 nThread=1){
    data.table::fwrite(dat, file_dir, 
                       sep = "\t", na = NA, quote = FALSE,
                       nThread = nThread)
}

