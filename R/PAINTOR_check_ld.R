PAINTOR_check_ld <- function(LD_matrix,
                             dat_ls,
                             locus_dir,
                             verbose = TRUE){
    if(is.null(LD_matrix)){
        return(NULL)
    }
    LD_ls <- PAINTOR_dat_to_list(dat = LD_matrix, 
                                 prefix = "LD")
    #### Name all LD according to PAINTOR convention ####
    names(LD_ls) <- paste0(paste0(basename(locus_dir),".ld"),
                           seq_len(length(LD_ls))) 
    #### Check for problems ####
    if(length(LD_ls)==1){
        if(length(LD_ls)!=length(dat_ls)){
            messager("A single LD_matrix will be applied",
                     "to all datasets.",v=verbose)
        }
    } else if(length(LD_ls)>1){
        if(length(LD_ls)!=length(dat_ls)){
            stp <- paste("When >1 LD_matrix is provided,",
                         "this must match the number of datasets in dat.")
            stop(stp)
        }
    }
    return(LD_ls)
}