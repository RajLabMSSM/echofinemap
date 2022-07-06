prepare_priors <- function(dat,
                           priors_col=NULL,
                           snp_col="SNP",
                           rescale_priors=TRUE, 
                           verbose=TRUE){ 
    if(is.null(priors_col)) return(NULL)
    if(!priors_col %in% colnames(dat)) {
        messager("priors_col not found in dat:",priors_col)
        return(NULL)
    }
    prior_weights <- dat[,priors_col, with=FALSE][[1]]
    messager("+ Utilizing prior_weights for",
             formatC(length(prior_weights), big.mark = ","),"SNPs.",
             v=verbose) 
    if(isTRUE(rescale_priors)){
        messager("+ Rescaling priors",v=verbose)
        prior_weights <- normalize_priors(x = prior_weights, 
                                          verbose = FALSE)
    }
    #### Name priors ####
    if(!snp_col %in% names(dat)){
        messager("snp_col=",snp_col," not in dat. Leving priors unnamed.")
    } else { 
        prior_weights <- stats::setNames(prior_weights, 
                                         dat[, snp_col, with=FALSE][[1]])
    }  
    #### Fill NAs ####
    if(sum(is.na(prior_weights))>0){
        messager("Warning: prior_weights can't have NAs.",
                 "Replacing with 0.")
        prior_weights[is.na(prior_weights)] <- 0
    }
    #### Check after processing ####
    if(length(prior_weights)!=nrow(dat)) {
        stop("prior_weights must be the same length ",
             "as the number of rows in dat.")
    } 
    # if(is.null(names(prior_weights))) {
    #     stop("All prior_weights must have RSIDs as names.")
    # }
    if(!is.numeric(prior_weights)) {
        stop("All prior_weights must be numeric.")
    }
    return(prior_weights)
}
