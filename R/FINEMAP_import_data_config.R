FINEMAP_import_data_config <- function(locus_dir,
                                       credset_thresh=.95,
                                       pvalue_thresh=.05,
                                       finemap_version=package_version("1.4.1"),
                                       top_config_only=FALSE,
                                       return_max_causal=FALSE,
                                       verbose=TRUE){
    prob <- pvalue <- NULL;
    # NOTES
    ## .config files: Gives all model results for all the configurations tested
    ## (regardless of whether they're over the 95% probability threshold)
    messager("Importing configuration probabilities (.config file).",
             v=verbose)
    config_path <- file.path(locus_dir,"FINEMAP/data.config")
    config_dat <- data.table::fread(config_path, nThread=1)   
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
    ## So that the same config probability
    ## is assigned to each SNP within that configuration.
    config_dat_melt <- lapply(seq_len(nrow(config_dat)), function(i){
        ROW <- config_dat[i,]
        snps <- strsplit(ROW$config,",")[[1]]
        data.table::data.table(SNP=snps, 
                               prob=as.numeric(rep(ROW$prob,length(snps))),
                               k=rep(length(snps),length(snps)))
    }) %>% data.table::rbindlist() 
    #### Post-process #####
    if(nrow(config_dat_melt)>0){
        ## "k" col was only added in FINEMAP >=1.4
        if("k" %in% names(config_dat)){
            max_causal <- max(config_dat$k)
        } else {
            max_causal <- max(config_dat_melt$k)
        }
        config_dat_melt <- dplyr::select(config_dat_melt, -k)
    } else {
        #### Report #### 
        messager("FINEMAP was unable to identify any credible sets at",
                 paste0("PP>=",credset_thresh),v=verbose)
    }
    #### Return ####
    if(return_max_causal){
        list(config_dat=config_dat_melt,
             max_causal=max_causal)
    } else {
        return(config_dat_melt)
    } 
}
