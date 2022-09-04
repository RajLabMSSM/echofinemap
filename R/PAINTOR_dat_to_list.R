PAINTOR_dat_to_list <- function(dat,
                                prefix="dataset"){
    if(methods::is(dat,"data.frame") | 
       methods::is(dat,"matrix")){
        dat_ls <- list(dat)
    } else if (is.list(dat)){
        dat_ls <- dat
    } else {
        stp <- paste("PAINTOR:: dat must be one of:",
                     paste0("\n - ",c("data.table",
                                      "(sparse) matrix",
                                      "(named) list"), 
                            collapse = "")
                     )
        
        stop(stp)
    }
    if(all(is.null(names(dat_ls)))){
        names(dat_ls) <- paste0(prefix,seq_len(length(dat_ls)))
    } else {
        names(dat_ls) <- make.unique(names(dat_ls))
    } 
    return(dat_ls)
}