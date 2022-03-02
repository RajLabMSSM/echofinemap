#' Fine-map handler
#' 
#' Handle fine-mapping across multiple tools.
#' 
#' @family finemapping functions
#' @export
finemap_handler <- function(locus_dir,
                            fullSS_path,
                            finemap_methods=c("SUSIE","FINEMAP"),
                            finemap_args=NULL,
                            dat,
                            dataset_type="GWAS",
                            force_new_finemap=TRUE,
                            LD_reference=NULL,
                            LD_matrix=NULL,
                            n_causal=5,
                            sample_size=NULL,
                            conditioned_snps=NULL,
                            # snp_col="SNP",
                            # freq_col="Freq",
                            # effect_col="Effect",
                            # stderr_col="StdErr",
                            # pval_col="P",
                            # N_cases_col="N_cases",
                            # N_controls_col="N_controls",
                            # A1_col="A1",
                            # A2_col="A2",
                            PAINTOR_QTL_datasets=NULL,
                            PP_threshold=.95,
                            consensus_threshold=2,
                            case_control=TRUE,
                            conda_env="echoR",
                            nThread=1,
                            verbose=TRUE){
    start_FM <- Sys.time()
    set.seed(1)
    # First, check if there's more than one fin-mapping method given. If so, switch to multi-finemap function
    # if(length(finemap_methods)>1){
    ## Next, see if fine-mapping has previously been done (with multi-finemap)
    file_path <- create_method_path(locus_dir = locus_dir,
                                    LD_reference = LD_reference,
                                    finemap_method = "Multi-finemap",
                                    compress = TRUE)
    ### If so, import the previous results
    if(file.exists(file_path) & force_new_finemap==FALSE){
        messager("++ Previously multi-finemap results identified. Importing:",file_path, v=verbose)
        finemap_dat <- data.table::fread(file_path, nThread=nThread)
    } else {
        ### If not, or if forcing new fine-mapping is set to TRUE, fine-map using multiple tools
        finemap_methods <- check_necessary_cols(dat = dat,
                                                finemap_methods = finemap_methods,
                                                sample_size = sample_size,
                                                dataset_type = dataset_type,
                                                verbose = verbose)
        finemap_dat <- multi_finemap(locus_dir = locus_dir,
                                     fullSS_path = fullSS_path,
                                     finemap_method_list = finemap_methods,
                                     finemap_args = finemap_args,
                                     dat = dat,
                                     dataset_type = dataset_type,
                                     LD_matrix = LD_matrix,
                                     n_causal = n_causal,
                                     sample_size = sample_size,
                                     
                                     # snp_col = snp_col,
                                     # freq_col = freq_col,
                                     # effect_col = effect_col,
                                     # stderr_col = stderr_col,
                                     # pval_col = pval_col,
                                     # N_cases_col = N_cases_col,
                                     # N_controls_col = N_controls_col,
                                     # A1_col = A1_col,
                                     # A2_col = A2_col,
                                     PAINTOR_QTL_datasets = PAINTOR_QTL_datasets,
                                     PP_threshold = PP_threshold,
                                     case_control = case_control,
                                     
                                     verbose = verbose,
                                     nThread = nThread,
                                     conda_env = conda_env)
        finemap_dat <- echodata::find_consensus_snps(dat = finemap_dat,
                                                     credset_thresh = PP_threshold,
                                                     consensus_thresh = consensus_threshold,
                                                     verbose = verbose)
        save_finemap_results(finemap_dat, file_path)
    }
    end_FM <- Sys.time()
    messager("+ Fine-mapping with '", paste0(finemap_methods, collapse=", "),"' completed:",v=verbose)
    print(round(end_FM-start_FM,2))
    return(finemap_dat)
}
