SUSIE_extract_cs <- function(dat,
                             fitted_bhat,
                             credset_thresh,
                             return_all_CS=TRUE,
                             verbose=TRUE){
    messager("+ SUSIE:: Extracting Credible Sets.",v=verbose)
    ## Get PIP
    dat$PP <- susieR::susie_get_pip(fitted_bhat)
    ## Get CS assignments
    CS_indices <- susieR::susie_get_cs(fitted_bhat)$cs
    susie_snps <- names(fitted_bhat$X_column_scale_factors)
    CS <- lapply(CS_indices, function(x){susie_snps[x]})
    CS_dict <- list()
    len <- if(isTRUE(return_all_CS)) length(CS) else 1
    ## Assign a CS number for each group of SNPs
    for(i in seq_len(len)){
        for(s in CS[[i]]){
            CS_dict <- append(CS_dict, stats::setNames(i,s))
        }
    }
    # Assign each SNP a CS group if it meets the PP threshold
    dat$CS <- lapply(dat$SNP, function(x){
        if(x %in% names(CS_dict) & subset(dat, SNP==x)$PP>=credset_thresh){
            CS_dict[[x]]
        } else{0}}) |> unlist()
    return(dat)
}
