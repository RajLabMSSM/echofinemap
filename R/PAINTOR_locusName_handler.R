#' Construct locus name for PAINTOR
#'
#' @keywords internal
PAINTOR_locusName_handler <- function(locus_name=NULL,
                                      locus,
                                      GWAS_datasets=NULL,
                                      QTL_datasets=NULL){
    if(is.null(locus_name)){
        locus_name <- paste(locus,GWAS_datasets,
                            paste(QTL_datasets,collapse = "--"), sep = ".")
        return(locus_name)
    }
    return(locus_name)
}