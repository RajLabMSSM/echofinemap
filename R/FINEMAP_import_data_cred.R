FINEMAP_import_data_cred <- function(locus_dir, 
                                     credset_thresh=.95,
                                     pvalue_thresh=.05,
                                     finemap_version=package_version("1.4.1"),
                                     agg_func=function(x){mean(x,na.rm=TRUE)},
                                     verbose=TRUE){
    CS <- PP <- SNP <- NULL;
    
    # NOTES:
    ## .cred files: Conditional posterior probabilities that 
    ## a given variant is causal conditional on the other causal variants 
    ## in the region.
    ## ---------------------------------------------------------##
    # !!IMPORTANT!!: Must search for .cred files without assuming 
    ## exact file names, because in FINEMAP>=1.4 CS file started being saved 
    ## as "data.cred<n_causal>". Whereas in FINEMAP<=1.3.1, CS are always saved 
    ## as "data.cred" (without any suffix). 
    cred_path <- list.files(file.path(locus_dir,"FINEMAP"),
                            "^data.cred*", full.names = TRUE)
    messager(length(cred_path),
             "data.cred* file(s) found in the same subfolder.")
    #### Choose cred_path ####
    ## If multiple data.cred* files are in the same directory, search for the 
    ## one that matches the n_causal specified by the user.
    ## If none using that naming scheme is found, just pick one arbitrarily.
     
    #### Continue ####
    ### Parse logs ####
    logs <- FINEMAP_import_log(locus_dir = locus_dir,
                               config_thresh = 0)
    #### Select accepted k suffixes ###
    ## IMPORTANT!: Whether you include data.cred* from all runs
    ## or just the most recent one will affect the results.
    ## Determining the correct file is a bit tricky.
    ## Should either be postPr_k (optimized k determined by FINEMAP)
    ## or n_causal (max number of causal SNPs specified by user). 
    # causal_k <- logs$causal_k
    # causal_k <- logs$n_causal
    # causal_k <- logs$priorPr_k
    causal_k <- logs$postPr_k
    
    
    ## FINEMAP estimates the most likely number of causal SNPs.
    ## In FINEMAP >=v1.4, it names the data.cred file after the ESTIMATED
    ## number of causal SNPs 
    ## (not necessarily the number that the user specified). 
    if(finemap_version>="1.4"){ 
        cred_path <- lapply(cred_path, function(cp){
            if(any(basename(cp)==paste0("data.cred",causal_k))){
                return(cp)
            } else{NULL}
        }) |> unlist() 
        causal_k <- as.integer(gsub("data.cred","",basename(cred_path)))
        cred_path <- stats::setNames(cred_path, causal_k)
    } else {
        cred_path <- cred_path[
            basename(cred_path)=="data.cred"
        ] 
        causal_k <- causal_k[seq_len(length(cred_path))]
        cred_path <- stats::setNames(cred_path, causal_k)
    } 
    messager("Selected file based on postPr_k:",
             paste(basename(cred_path),collapse = ","),
             v=verbose)
    
    messager("Importing conditional probabilities (.cred file).",
             v=verbose)
    # !!IMPORTANT!!: Must include skip=index when reading in file, 
    ## because FINEMAP>=1.4 has additional comment rows before this, whereas 
    ## FINEMAP<=1.3.1 does not. 
    cred_dat <- lapply(names(cred_path), FUN = function(k){
        d <- data.table::fread(input = cred_path[[k]],
                          skip = "index", 
                          na.strings = c("<NA>","NA"),
                          nThread = 1)
        # #### Restructure data to SNP-wise table format ####
        cred_cols <- grep("cred*", colnames(d), value = TRUE)
        prob_cols <- grep("prob*", colnames(d), value = TRUE)
        CS <- lapply(seq_len(nrow(d)), function(i){
            rsids <- d[i,cred_cols, with=FALSE]
            PP_vals <- d[i,prob_cols, with=FALSE]
            data.table::data.table(SNP=unname(t(rsids)[,1] ),
                                   PP=unname(t(PP_vals)[,1]),
                                   CS=d$index[i],
                                   k=k) 
        }) |> data.table::rbindlist() |>
            subset(!is.na(SNP)) 
        return(CS)
    }) |> data.table::rbindlist()
    #### Ensure 1 line per SNP ####
    cred_dat <- dplyr::group_by(cred_dat, SNP) |>
        dplyr::summarise(PP=agg_func(PP),
                         CS=agg_func(CS),
                         k=paste(k,collapse = ";")) |>
        data.table::data.table()
    
    #### Assign credible set number ####
    #### Check is any configurations met the criterion ####
    config_dat <- FINEMAP_import_data_config(locus_dir = locus_dir,
                                             credset_thresh = credset_thresh,
                                             finemap_version = finemap_version,
                                             pvalue_thresh = pvalue_thresh,
                                             top_config_only = FALSE,
                                             verbose = FALSE) 
    if(nrow(config_dat)==0){ 
        ## If the configuration itself does not meet the PP threshold, 
        ## then none of the SNPs should be counted
        ## as part of the 95% credible set.
        messager("No configurations were causal",
                 paste0("at PP>=",credset_thresh,"."),v=verbose)
        cred_dat[,CS:=0]
    }else {
        ## If the conditional PP of the SNP does not meet the PP threshold,
        ## then don't assign it to a 95% credible set.
        cred_dat[CS<credset_thresh,] <- 0 
    }   
    return(cred_dat)
}
