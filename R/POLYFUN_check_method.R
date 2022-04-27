POLYFUN_check_method <- function(method,
                                 verbose){
    opts <- c("SUSIE","FINEMAP")
    method <- unique(toupper(method))
    method <- method[method %in% opts]
    method <- method[1]
    if(length(method)==0 | is.na(method)){
        stp <- paste0(
            "PolyFun:: The method argument must be one of the following:\n",
            paste(" -",opts,collapse="\n")
        )
        stop(stp)
    }
    messager("PolyFun:: Fine-mapping with",paste0("method=",method),
             v=verbose)
    return(method)
}