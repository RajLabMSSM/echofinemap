finemap_args_interpreter <- function(finemap_args,
                                     method="FINEMAP"){
    if(method %in% names(finemap_args)){
        args_list <- finemap_args[[method]]
        if(method=="FINEMAP") return(args_list)
        
    } else {return(NULL)}
}
