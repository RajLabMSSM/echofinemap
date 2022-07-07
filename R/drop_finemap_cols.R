#' Drop fine-mapping columns
#' 
#' Drop all method-specific and/or multi-tool summary columns from data.
#' This is helpful when ensuring duplicate columns aren't created when 
#' re-running fine-mapping with the same method.
#' 
#' @param summary_cols Multi-tool summary columns to drop.
#' @inheritParams multifinemap
#' @export
#' @examples 
#' dat2 <- echofinemap::drop_finemap_cols(dat=dat)
drop_finemap_cols <- function(dat, 
                              finemap_methods = NULL, 
                              summary_cols = c("Support",
                                               "Consensus_SNP",
                                               "mean.PP"),
                              generic_cols = c("CS",
                                               "PP",
                                               "k",
                                               "PP_snp",
                                               "PP_config"),
                              verbose = TRUE){
    if(is.null(finemap_methods)) {
        finemap_methods <- lfm()
    }
    #### Find existing cols #####
    drop_cols <- grep(colnames(dat), 
                      pattern = paste(c(paste0("^",finemap_methods),
                                        summary_cols,
                                        generic_cols),
                                      collapse = "|"),value = TRUE)  
    dat2 <- data.table::copy(dat)
    #### Drop existing cols and report #####
    if(length(drop_cols)>0) {
        messager("Dropping",length(drop_cols),
                 "pre-existing fine-mapping columns.",
                 v=verbose)
        dat2[,(drop_cols):=NULL]
    }
    return(dat2)
}
