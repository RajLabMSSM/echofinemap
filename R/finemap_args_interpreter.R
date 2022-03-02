finemap_args_interpreter <- function(finemap_args,
                                     method="FINEMAP"){
    # method <- "SUSIE"
    # finemap_args=list("FINEMAP"=list("--n-threads"=4),
    #                   "SUSIE"=list("scaled_prior_variance"=.001,
    #                                "max_iter"=100,
    #                                "doofus"=42))
    if(method %in% names(finemap_args)){
        args_list <- finemap_args[[method]]
        if(method=="FINEMAP") return(args_list)
        
    } else {return(NULL)}
}
