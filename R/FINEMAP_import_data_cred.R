FINEMAP_import_data_cred <- function(locus_dir,
                                     n_causal,
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
    if(length(cred_path)>1){
        messager("Multiple data.cred* files found in the same subfolder.")
        n_causal.match <- cred_path[
            basename(cred_path)==paste0("data.cred",n_causal)
        ][1]
        if(length(n_causal.match)>0){ 
            cred_path <- n_causal.match
            messager("Choosing", basename(cred_path),
                     "due to matching n_causal SNPs.",
                     v=verbose)
        }else {
            cred_path <- cred_path[1]
            messager("Choosing", basename(cred_path),"arbitarily.",
                     v=verbose)
        } 
    }
    
    ## ---------------------------------------------------------##
    
    ## ---------------------------------------------------------##
    # !!IMPORTANT!!: Must include skip=index when reading in file, 
    ## because FINEMAP>=1.4 has additional comment rows before this, whereas 
    ## FINEMAP<=1.3.1 does not. 
    data.cred <- data.table::fread(cred_path,
                                   skip = "index", 
                                   na.strings = c("<NA>","NA"),
                                   nThread = 1)
    ## ---------------------------------------------------------##
    
    cred.cols <- grep("cred*", colnames(data.cred), value = TRUE)
    prob.cols <- grep("prob*", colnames(data.cred), value = TRUE)
    #### Restructure data to SNP-wise table format ####
    CS <- lapply(seq_len(nrow(data.cred)), function(i){
        rsids <- subset(data.cred, select=cred.cols)[i,]
        PP_vals <- subset(data.cred, select=prob.cols)[i,]
        cred_sets <- data.table::data.table(SNP=unname( t(rsids)[,1] ),
                                            PP=unname(t(PP_vals)[,1]),
                                            CS=i)
        return(cred_sets)
    }) %>% data.table::rbindlist() %>%
        subset(!is.na(SNP))
    return(CS)
}
