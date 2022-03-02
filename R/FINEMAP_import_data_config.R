FINEMAP_import_data_config <- function(locus_dir,
                                       credset_thresh=.95,
                                       pvalue_thresh=.05,
                                       top_config_only=TRUE,
                                       verbose=TRUE){
    # NOTES
    ## .config files: Gives all model results for all the configurations tested
    ## (regardless of whether they're over the 95% probability threshold)
    messager("+ FINEMAP:: Importing top configuration probability (.config)...",
             v=verbose)
    config_path <- file.path(locus_dir,"FINEMAP/data.config")
    data.config <- data.table::fread(config_path, nThread=1)
    if(top_config_only){
        data.config <- data.config[1,]
    }
    # Gaurd against future renaming of columns
    if(!is.null(credset_thresh) & "prob" %in% colnames(data.config)){
        data.config <- subset(data.config, prob>=credset_thresh)
    }
    # Not all FINEMAP versions seem to have this "pvalue" column?
    if(!is.null(pvalue_thresh) & "pvalue" %in% colnames(data.config)){
        data.config <- subset(data.config, pvalue<pvalue_thresh)
    }
    # Restructure config file
    ## Use the probability of the configuration itself as the 
    ## snp-wise probabilities
    data.config_format <- data.frame(SNP=strsplit(data.config$config,
                                                  ",")[[1]],
                                     PP=data.config$prob,
                                     CS=1)
    return(data.config_format)
}
