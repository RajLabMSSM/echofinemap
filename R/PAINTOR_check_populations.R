PAINTOR_check_populations <- function(dat_ls,
                                      LD_ls = NULL,
                                      populations = NULL,
                                      verbose = TRUE){
    if(is.null(LD_ls)){ 
        messager("No LD_matrix provided.",
                 "Will compute LD from refrence panel instead.",v=verbose)
        if(length(populations)==0){
            stp <- "Must provide >0 populations."
            stop(stp)
        } else if(length(populations)==1){
            messager("Assigning one population to all datasets:",populations,
                     v=verbose)
            populations <- rep(populations,length(dat_ls))
        } else if(length(populations)>1){
            if(length(populations)!=length(dat_ls)){
                stp2 <- paste("populations must be the same length",
                              "as the number of datasets.")
                stop(stp2)
            } 
        }
        return(populations)
    } else {
        messager("Using LD matrices provided in LD_matrix argument.",v=verbose)
        return(NULL)
    } 
}