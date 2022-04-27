prepare_priors <- function(prior_weights,
                           rescale_priors,
                           dat,
                           verbose=TRUE){
    if(!is.null(prior_weights)){
        # prior_weights <- prior_weights[keep_i]
        messager("+ Utilizing prior_weights for",
                 formatC(length(prior_weights), big.mark = ","),"SNPs.",
                 v=verbose)
        if(isTRUE(rescale_priors)){
            messager("+ Rescaling priors",v=verbose)
            prior_weights <- normalize_priors(x = prior_weights)
        }
        if(length(prior_weights)!=nrow(dat)) {
            stop("prior_weights must be the same length ",
                 "as the number of rows in dat.")
        }
    }
    return(prior_weights)
}
