PAINTOR_check_required_cols <- function(dat,
                                        zscore_col,
                                        tstat_col){ 
    
    if(is.null(dat))return(dat)
    dat_ls <- PAINTOR_dat_to_list(dat = dat)
    dat_ls <- lapply(dat_ls, 
                     function(d){
        #### Check columns ####                 
        if(!zscore_col %in% names(d)){
            stp <- paste("zscore_col",paste0("(",zscore_col,")"),
                         "must be present in dat.")
            stop(stp)
        } else {
            data.table::setnames(d,zscore_col,"ZSCORE")
        }
        if(!tstat_col %in% names(d)){
            #### Try to impute tstat ####
            d <- echodata::standardize(d) 
            if(!tstat_col %in% names(d)){
                stp <- paste("tstat_col",paste0("(",tstat_col,")"),
                             "must be present in dat.")
                stop(stp)
            } 
        } else {
            data.table::setnames(d,tstat_col,"tstat")
        } 
        return(d)
    })
   return(dat_ls)
}
