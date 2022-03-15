POLYFUN_check_method <- function(method,
                                 verbose){
    opts <- c("SUSIE","FINEMAP")
    method <- unique(tolower(method))
    method <- method[method %in% opts]
    if(length(mode)==0){
        stp <- paste0(
            "PolyFun:: The method argument must be one of the following:\n",
            paste(" -",opts,collapse="\n")
        )
        stop(stp)
    }
    method <- method[1]
    messager("PolyFun:: Fine-mapping with",paste0("method=",method),
             v=verbose)
    return(method)
}