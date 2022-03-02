FINEMAP_import_data_snp <- function(locus_dir,
                                    credset_thresh=.95,
                                    prob_col="prob",
                                    verbose=TRUE){
    # NOTES:
    ## .snp files: Posterior probabilities in this file are the marginal
    ## posterior probability that a given variant is causal.
    
    # Prob column descriptions:
    ## prob: column the marginal Posterior Inclusion Probabilities (PIP). 
    ## The PIP for the l-th SNP is the posterior probability that this SNP
    ## is causal.
    ## prob_group: the posterior probability that there is at least one
    ## causal signal among SNPs in the same group with this SNP.
    
    messager("+ FINEMAP:: Importing",prob_col,"(.snp)", v=verbose)
    data.snp <- data.table::fread(file.path(locus_dir,"FINEMAP/data.snp"),
                                  nThread = 1)
    data.snp <- data.snp[data.snp[[prob_col]] > credset_thresh,] %>%
        plyr::mutate(CS=1) %>%
        dplyr::rename(PP=dplyr::all_of(prob_col))
    if(!"prob" %in% colnames(data.snp)){
        data.snp$prob <- data.snp$prob_group
    }
    return(data.snp)
}
