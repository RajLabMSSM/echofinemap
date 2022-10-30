#' Check arguments
#' 
#' Check a list of arguments for a given function and:
#' \itemize{
#' \item{Remove any that do not match available arguments for that method.}
#' \item{Set default values for any arguments that are NULL or not provided.}
#' }
#' @param max_values Max number of values to return for any argument.
#' @inheritParams multifinemap
#' @keywords internal
#' @returns Named list of arguments for a specific \code{finemap_method}.
#' 
#' @source 
#' \code{
#' finemap_args <- list(SUSIE=list("typo1"=1,rescale_priors=TRUE),
#'                       FINEMAP=list("FINEMAP_path"=NULL,"model"=NULL))
#' fma <- check_args(finemap_args=finemap_args, finemap_method="SUSIE")
#' }
check_args <- function(finemap_args, 
                       finemap_method, 
                       max_values=1,
                       verbose=TRUE){
    
    force(finemap_args)
    force(finemap_method)
    #### Check methods ####
    finemap_method <- lfm(finemap_methods = finemap_method,
                          verbose = FALSE)
    finemap_args <- finemap_args[
        lfm(finemap_methods = names(finemap_args),
            verbose = FALSE)
    ] 
    #### Select appropriate function name ####
    fun <- if(grepl("^POLYFUN",finemap_method)){
        "POLYFUN"
    } else {
        finemap_method
    } 
    #### Get defaults for function and evaluate them ####
    fm <- formals(fun)
    fm_eval <- lapply(fm,
                      function(x){
        if(methods::is(x,"name") &&
           methods::is(x,"language") &&
           methods::is(x,"refObject")){
            return("REMOVE_ME!!")
        }else {
            return(eval(x))
        }
    })
    fm_eval <- fm_eval[fm_eval!="REMOVE_ME!!"]
    #### Check which user-supplied args are actually real args ####
    method_args <- finemap_args[[finemap_method]] 
    method_args <- method_args[names(method_args) %in% names(fm)]
    #### If the user didn't supply any arguments for this method, 
    ## simply return the defaults for the respective function.
    if(length(method_args)==0){ 
        messager("Using all default values for",
                 paste0("finemap_args$",finemap_method),v=verbose)
        return(fm_eval)
    }  
    print_val <- function(a,val){
        paste0(a,"=",
               if(is.character(val)){
                   shQuote(val)
               }else{val}) 
    }
    #### Otherwise, iteratively go through each arg ####
    fma <- lapply(stats::setNames(names(fm_eval),
                                  names(fm_eval)), 
           function(a){
       user_val <- method_args[[a]]
       if(a %in% names(method_args) && 
          !is.null(user_val)){
           val <- user_val
           messager("Setting argument:",
                    print_val(a,val),
                    v=verbose)
           #### Limit returns values ###
           if(length(val)>1) val <- val[seq_len(max_values)]
           return(val)
       } else {
           if(is.null(user_val)) {
               val <- eval(fm[[a]])
               ## Only need to report if the default isn't NULL already
               if(!is.null(val)){
                   messager("Setting argument to default:",
                            print_val(a,val),v=verbose)
               } 
           } else {
               val <- user_val
           }
           #### Limit returns values ###
           if(length(val)>1) val <- val[seq_len(max_values)]
           return(val)
       }    
    })
    return(fma)
}
