PAINTOR_check_superpopulation <- function(dat_ls,
                                      LD_ls = NULL,
                                      superpopulation = NULL,
                                      verbose = TRUE){
    if(is.null(LD_ls)){ 
        messager("No LD_matrix provided.",
                 "Will compute LD from refrence panel instead.",v=verbose)
        if(length(superpopulation)==0){
            stp <- "Must provide >0 superpopulation."
            stop(stp)
        } else if(length(superpopulation)==1){
            messager("Assigning one population to all datasets:",superpopulation,
                     v=verbose)
            superpopulation <- rep(superpopulation,length(dat_ls))
        } else if(length(superpopulation)>1){
            if(length(superpopulation)!=length(dat_ls)){
                stp2 <- paste("superpopulation must be the same length",
                              "as the number of datasets.")
                stop(stp2)
            } 
        }
        return(superpopulation)
    } else {
        messager("Using LD matrices provided in LD_matrix argument.",v=verbose)
        return(NULL)
    } 
}
