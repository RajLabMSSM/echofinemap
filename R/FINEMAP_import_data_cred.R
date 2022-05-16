FINEMAP_import_data_cred <- function(locus_dir, 
                                     credset_thresh=.95,
                                     pvalue_thresh=.05,
                                     finemap_version=package_version("1.3"),
                                     verbose=TRUE){
    # NOTES:
    ## .cred files: Conditional posterior probabilities that 
    ## a given variant is causal conditional on the other causal variants 
    ## in the region.
    messager("Importing conditional probabilities (.cred)",
             v=verbose)
    ## ---------------------------------------------------------##
    # !!IMPORTANT!!: Must search for .cred files without assuming 
    ## exact file names, because in FINEMAP>=1.4 CS file started being saved 
    ## as "data.cred<n_causal>". Whereas in FINEMAP<=1.3.1, CS are always saved 
    ## as "data.cred" (without any suffix). 
    cred_path <- list.files(file.path(locus_dir,"FINEMAP"),
                            "^data.cred*", full.names = TRUE)
    #### Choose cred_path ####
    ## If multiple data.cred* files are in the same directory, search for the 
    ## one that matches the n_causal specified by the user.
    ## If none using that naming scheme is found, just pick one arbitrarily.
    
    #### Check is any configurations met the criterion ####
    config_dat <- FINEMAP_import_data_config(locus_dir = locus_dir,
                                             credset_thresh = credset_thresh,
                                             finemap_version = finemap_version,
                                             pvalue_thresh = pvalue_thresh,
                                             top_config_only = FALSE,
                                             verbose = verbose) 
    #### Continue ####
    ### Parse logs ####
    logs <- FINEMAP_import_log(locus_dir = locus_dir)
    causal_k <- logs$causal_k
    
    ## FINEMAP estimates the most likely number of causal SNPs.
    ## In FINEMAP >=v1.4, it names the data.cred file after the ESTIMATED
    ## number of causal SNPs 
    ## (not necessarily the number that the user specified). 
    if(length(cred_path)>1){
        messager(length(cred_path),
                 "data.cred* files found in the same subfolder.")
        cred_path <- cred_path[
            basename(cred_path)==paste0("data.cred",causal_k)
        ] 
        messager("Selecting the data.cred* file with the greatest",
                 "posterior probability of k causal SNPs:",
                 basename(cred_path))
    }
    # !!IMPORTANT!!: Must include skip=index when reading in file, 
    ## because FINEMAP>=1.4 has additional comment rows before this, whereas 
    ## FINEMAP<=1.3.1 does not. 
    cred_dat <-  data.table::fread(input = cred_path,
                                    skip = "index", 
                                    na.strings = c("<NA>","NA"),
                                    nThread = 1)
    select_cols <- paste0(c("cred","prob"),causal_k)
    cred_dat <- cred_dat[,select_cols, with=FALSE]
    data.table::setnames(cred_dat,c("SNP","PP"))
    ## Assign credible set number 
    if(nrow(config_dat)==0){
        cred_dat[,CS:=0]
    }else {
        cred_dat <- dplyr::mutate(cred_dat, 
                                  CS=ifelse(PP>=credset_thresh,1,0))
    }  

    # #### Restructure data to SNP-wise table format ####
    # cred.cols <- grep("cred*", colnames(cred_dat), value = TRUE)
    # prob.cols <- grep("prob*", colnames(cred_dat), value = TRUE)    
    # CS <- lapply(seq_len(nrow(cred_dat)), function(i){
    #     rsids <- subset(cred_dat, select=cred.cols)[i,]
    #     PP_vals <- subset(cred_dat, select=prob.cols)[i,]
    #     cred_sets <- data.table::data.table(SNP=unname(t(rsids)[,1] ),
    #                                         PP=unname(t(PP_vals)[,1]),
    #                                         CS=i)
    #     return(cred_sets)
    # }) %>% data.table::rbindlist() %>%
    #     subset(!is.na(SNP))
    return(cred_dat)
}
