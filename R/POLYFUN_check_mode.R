POLYFUN_check_mode <- function(mode,
                               verbose=TRUE){
    
    opts <- eval(formals(POLYFUN)$mode) 
    if(is.null(mode)){
        mode <- mode[[1]]
        messager("PolyFun:: mode=NULL. Setting to default:",
                 shQuote(mode),v=verbose)
        return(mode)
    }
    mode <- unique(tolower(mode))
    mode <- mode[mode %in% opts]
    if(length(mode)==0){
        stp <- paste0(
            "PolyFun:: The mode argument must be one of the following:\n",
            paste(" -",opts,collapse="\n")
            )
        stop(stp)
    }
    mode <- mode[[1]]
    messager("PolyFun:: Using priors from",paste0("mode=",mode),v=verbose)
    return(mode)
}