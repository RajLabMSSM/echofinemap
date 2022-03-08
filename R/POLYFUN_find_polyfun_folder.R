#' Find PolyFun folder
#' 
#' Folder where PolyFun submodule is stored.
#' @keywords internal
#' @family polyfun
#' @source
#' \code{
#' polyfun_path <- POLYFUN_find_polyfun_folder()
#' }
POLYFUN_find_polyfun_folder <- function(polyfun_path=NULL){
    if(is.null(polyfun_path)){
        polyfun_path <- system.file("tools/polyfun",
                                    package = "echofinemap")
    }
    return(polyfun_path)
}
