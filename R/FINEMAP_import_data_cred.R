FINEMAP_import_data_cred <- function(locus_dir,
                                     verbose=TRUE){
    # NOTES:
    ## .cred files: Conditional posterior probabilities that 
    ## a given variant is causal conditional on the other causal variants 
    ## in the region.
    messager("+ FINEMAP:: Importing conditional probabilities (.cred)",
             v=verbose)
    # cred_path <- file.path(locus_dir,"FINEMAP/data.cred")
    cred_path <- list.files(file.path(locus_dir,"FINEMAP"),
                            "data.cred", full.names = TRUE)
    #### Only use the first CS ####
    cred_path <- cred_path[1]
    data.cred <- data.table::fread(cred_path,
                                   na.strings = c("<NA>","NA"),
                                   nThread = 1)
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
