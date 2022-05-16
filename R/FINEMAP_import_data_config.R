FINEMAP_import_data_config <- function(locus_dir,
                                       credset_thresh=.95,
                                       pvalue_thresh=.05,
                                       finemap_version=package_version("1.4"),
                                       top_config_only=FALSE,
                                       return_max_causal=FALSE,
                                       verbose=TRUE){
    prob <- pvalue <- NULL;
    # NOTES
    ## .config files: Gives all model results for all the configurations tested
    ## (regardless of whether they're over the 95% probability threshold)
    messager("Importing configuration probabilities (.config)...",
             v=verbose)
    config_path <- file.path(locus_dir,"FINEMAP/data.config")
    config_dat <- data.table::fread(config_path, nThread=1) 
    max_causal <- max(config_dat$k)
    # sum_probs <- dplyr::group_by(config, config) %>% 
    #     dplyr::summarise(prob_sum=sum(prob))# %>% 
        # dplyr::filter(prob_sum>=credset_thresh) 
    # Guard against future renaming of columns
    if((!is.null(credset_thresh)) &
       ("prob" %in% colnames(config_dat))){ 
        config_dat <- config_dat[prob>=credset_thresh,]
    }
    # Not all FINEMAP versions seem to have this "pvalue" column?
    if((!is.null(pvalue_thresh)) &
       ("pvalue" %in% colnames(config_dat))){
        config_dat <- config_dat[pvalue<pvalue_thresh,]
    }
    if(top_config_only){ 
        config_dat <- dplyr::slice_max(.data = config_dat, 
                                       n=1, 
                                       order_by = prob) 
    }
    #### Restructure config file ####
    ## Only necessary for older versions of FINEMAP
    if(finemap_version<"1.4"){ 
        ## Use the probability of the configuration itself as the 
        ## snp-wise probabilities
        config_dat <- data.frame(SNP=strsplit(config_dat$config,",")[[1]],
                                         PP=config_dat$prob,
                                         CS=1) 
    }  
    #### Report ####
    if(nrow(config_dat)==0){
        messager("FINEMAP was unable to identify any credible sets at",
                 paste0("PP>=",credset_thresh),v=verbose)
    }
    if(return_max_causal){
        list(config_dat=config_dat,
             max_causal=max_causal)
    } else {
        return(config_dat)
    } 
}
