SUSIE_args_intrepreter <- function(args_list,
                                   arg_name){
    valid_args <- names(args_list)[names(args_list) %in% names(formals(SUSIE))]
    # if(length(args_list)>length(valid_args)){
    #   invalid_args <- names(args_list)[!names(args_list) %in% valid_args]
    #   warning(paste0(method,":: Ignoring unrecognized arguments: ",
    #                  paste(invalid_args,collapse=", ")))
    # }
    if(length(valid_args)>0) return(args_list)
}



