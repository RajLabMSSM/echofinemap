#' Get susieR fine-mapping function
#' 
#' The authors of \pkg{susieR} merged \code{susie_ss} and \code{susie_bhat}
#'  into \code{susie_suff_stat} in 11/2019.
#' @param verbose Print messages.
#' @keywords internal
#' @importFrom utils packageVersion
SUSIE_get_func <- function(verbose=TRUE){
    susie_version <- utils::packageVersion("susieR")
    if(length(find("susie_bhat"))==0){
        messager("+ SUSIE:: Using `susie_suff_stat()` from susieR",
                 paste0("v",susie_version),v=verbose)
        susie_func <- get("susie_suff_stat", asNamespace("susieR"))
    } else {
        messager("+ SUSIE:: Using `susie_bhat()` from susieR",
                 paste0("v",susie_version),v=verbose)
        susie_func <- get("susie_bhat", asNamespace("susieR"))
    }
}
