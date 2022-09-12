#' Find PolyFun folder
#' 
#' Folder where PolyFun submodule is stored.
#' @keywords internal
#' @family polyfun
#' @source
#' \code{
#' polyfun_path <- POLYFUN_find_folder()
#' }
POLYFUN_find_folder <- function(polyfun_path=NULL){
    if(is.null(polyfun_path)){
        polyfun_path <- system.file("tools/polyfun", 
                                    package = "echofinemap")
    }
    if(polyfun_path=="") stop("Cannot find polyfun_path.")
    return(polyfun_path)
}
