#' Prepare multi-ancestry LD files for PAINTOR
#'
#' @keywords internal
#' @importFrom stats setNames
PAINTOR_prepare_ld_multiancestry <- function(dat_merged,
                                             LD_ls,
                                             locus_dir,
                                             dat_populations,  
                                             PT_results_path,
                                             LD_reference = "1KGphase3",
                                             force_new_LD = FALSE,
                                             shared_snps_only = TRUE,
                                             fillNA = 0,
                                             nThread = 1,
                                             verbose = TRUE){
    
    locus_name <- basename(locus_dir)
    #### User-provided LD matrices ####
    if(!is.null(LD_ls)){
        messager("PAINTOR:: Preparing user-provided LD files.",v=verbose) 
        LD_ls <- lapply(LD_ls, function(ld_mat){
            echoLD::subset_common_snps(LD_matrix = ld_mat,
                                       dat = dat_merged,
                                       verbose = verbose)$LD
        }) 
    #### Download LD matrices from LD panel ####
    } else if(!is.null(dat_populations)){
        messager("PAINTOR:: Preparing LD files from LD_reference:",
                 LD_reference,v=verbose) 
        #### Only query LD as many times as necessary ####
        uniq_pops <- unique(dat_populations)
        if(length(uniq_pops)==1){
            messager("Only one unique population will be used:",uniq_pops,
                     v=verbose)
        }
        #### Query ref server ####
        LD_ls <- lapply(
            stats::setNames(uniq_pops,uniq_pops),
              function(pop){  
                  messager("+ PAINTOR::",pop,v=verbose)
                  echoLD::get_LD(locus_dir = locus_dir,
                                 query_dat = dat_merged, 
                                 LD_reference = LD_reference,
                                 superpopulation = pop,
                                 force_new_LD = force_new_LD,
                                 fillNA = fillNA,
                                 nThread = nThread,
                                 verbose = verbose)$LD 
              })
        #### Duplicate 
        if(length(dat_populations)>length(uniq_pops)){
            LD_ls <- lapply(dat_populations, 
                           function(pop){LD_ls[[pop]]})
        }
    } else {
        stp <- paste("Must provide one of the following sets of arguments:",
                     "\n - LD_matrix",
                     "\n - dat_populations and LD_reference")
        stop(stp)
    } 
    #### Name all LD according to PAINTOR convention ####
    names(LD_ls) <- paste0(paste0(locus_name,".ld"),seq_len(length(LD_ls))) 
    #### Filter to only SNPs shared between all LD matrices ####
    if(isTRUE(shared_snps_only)){
        shared_snps <- Reduce(intersect, lapply(LD_ls,colnames))
        shared_snps <- intersect(shared_snps, dat_merged$SNP)
        messager("+ PAINTOR::",formatC(length(shared_snps),big.mark = ","),
                 "shared SNPs identified.",v=verbose)
    } 
    ##### Filter SNPs and save LD ####
    ld_paths <- lapply(seq_len(length(LD_ls)), 
                       function(i){
        messager("Filtering LD matrix:",names(LD_ls)[i],v=verbose)
       ld_path <- file.path(PT_results_path, names(LD_ls)[i]) 
       if((!file.exists(ld_path)) | isTRUE(force_new_LD)){
           ld_mat <- LD_ls[[i]] 
           if(length(shared_snps)>0){
               ld_mat <- ld_mat[shared_snps, shared_snps]
           }
           messager("++ PAINTOR::",paste(formatC(dim(ld_mat),big.mark = ","),
                                         collapse=" x "),"LD matrix.",v=verbose) 
           #### Save ####
           messager("++ PAINTOR:: Writing LD file to ==> ",ld_path,v=verbose)
           data.table::fwrite(data.table::data.table(as.matrix(ld_mat)),
                              ld_path,
                              sep = " ", 
                              quote = FALSE,
                              col.names = FALSE, 
                              row.names = FALSE,
                              na = 0.0,
                              nThread = 1)
           } else {
               messager("Importing preexisting file:",ld_path,v=verbose)
           }
        return(ld_path)
    }) |> `names<-`(names(LD_ls))
    return(list(paths = ld_paths,
                shared_snps = shared_snps))
}
