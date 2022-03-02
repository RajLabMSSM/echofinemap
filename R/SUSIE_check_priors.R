SUSIE_check_priors <- function(prior_weights,
                               dat,
                               keep_i,
                               max_causal,
                               rescale_priors,
                               verbose=TRUE){
    messager("+ SUSIE:: max_causal =",max_causal, v=verbose)
    if(!is.null(prior_weights)){
        prior_weights <- prior_weights[keep_i]
        messager("+ SUSIE:: Utilizing prior_weights for",
                 formatC(length(prior_weights), big.mark = ","),"SNPs.",
                 v=verbose)
        if(rescale_priors){
            messager("+ SUSIE:: Rescaling priors",v=verbose)
            prior_weights <- prior_weights / sum(prior_weights, na.rm = TRUE)
        }
        if(length(prior_weights)!=nrow(dat)) {
            stop("prior_weights must be the same length ",
                 "as the number of rows in dat.")
        }
    }
    return(prior_weights)
}
