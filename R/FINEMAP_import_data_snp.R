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
    
    messager("Importing marginal probabilities (.snp file).", v=verbose)
    snp_dat <- data.table::fread(file.path(locus_dir,"FINEMAP/data.snp"),
                                  nThread = 1)
    if((!"prob" %in% names(snp_dat)) &&
       ("prob_group" %in% names(snp_dat))){
        snp_dat$prob <- snp_dat$prob_group
    }
    snp_dat <- snp_dat[snp_dat[[prob_col]] > credset_thresh,]
    return(snp_dat)
}
