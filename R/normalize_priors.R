normalize_priors <- function(x, 
                             fn = function(x){x/sum(x,na.rm = TRUE)},
                             verbose=TRUE){
    if(verbose){
        messager("  Normalizing priors with the function:")
        print(fn)
    } 
    fn(x)
}