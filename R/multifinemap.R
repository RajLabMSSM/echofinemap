#' Multi-fine-map
#' 
#' Handle fine-mapping across multiple tools.
#' 
#' @param priors_col [Optional] Name of the a column in 
#' \code{dat} to extract SNP-wise prior probabilities from.
#' @inheritParams echodata::get_sample_size
#' @inheritParams echodata::find_consensus_snps
#' @family finemapping functions
#' @export
#' @examples
#' dat <- echofinemap::drop_finemap_cols(dat = echodata::BST1)
#' LD_matrix <- echodata::BST1_LD_matrix
#' locus_dir <- file.path(tempdir(),echodata::locus_dir) 
#' 
#' dat2 <- echofinemap::multifinemap(dat = dat, 
#'                                  locus_dir = locus_dir,
#'                                  LD_matrix = LD_matrix)
multifinemap <- function(dat,
                         locus_dir,
                         fullSS_path=NULL,
                         finemap_methods=c("ABF","SUSIE","FINEMAP"),
                         finemap_args=NULL,
                         dataset_type="GWAS",
                         force_new_finemap=FALSE,
                         LD_reference=NULL,
                         LD_matrix=NULL,
                         n_causal=5,
                         compute_n="ldsc",
                         standardise_headers=FALSE,
                         conditioned_snps=NULL,
                         PAINTOR_QTL_datasets=NULL,
                         credset_thresh=.95,
                         consensus_thresh=2,
                         case_control=TRUE,
                         priors_col=NULL,
                         conda_env="echoR_mini",
                         nThread=1,
                         seed=2022,
                         verbose=TRUE){ 
    start_FM <- Sys.time()
    set.seed(seed)  
    ## See if fine-mapping has previously been done.
    file_path <- create_method_path(locus_dir = locus_dir,
                                    LD_reference = LD_reference,
                                    finemap_method = "Multi-finemap",
                                    compress = TRUE)
    #### If so, import the previous results ####
    if(file.exists(file_path) & force_new_finemap==FALSE){
        messager("++ Previously multi-finemapped results identified.",
                 "Importing:",file_path, v=verbose)
        dat2 <- data.table::fread(file_path, nThread=nThread)
    
    ### Otherwise, continue fine-mapping ####
    } else {
        ## Get sample size and standarize colnames at same time.
        dat <- echodata::get_sample_size(
            dat = dat, 
            compute_n = compute_n,
            standardise_headers = standardise_headers,
            verbose = verbose)  
        ## If not, or if forcing new fine-mapping is set to TRUE, 
        ## fine-map using multiple tools
        finemap_methods <- check_required_cols(
            dat = dat,
            finemap_methods = finemap_methods, 
            dataset_type = dataset_type,
            verbose = verbose)  
        dat2 <- multifinemap_handler(
            dat = dat,
            locus_dir = locus_dir,
            fullSS_path = fullSS_path,
            finemap_methods = finemap_methods,
            finemap_args = finemap_args, 
            dataset_type = dataset_type,
            force_new_finemap = force_new_finemap,
            LD_matrix = LD_matrix,
            n_causal = n_causal,
            compute_n = compute_n, 
            conditioned_snps = conditioned_snps,
            PAINTOR_QTL_datasets = PAINTOR_QTL_datasets,
            credset_thresh = credset_thresh,
            case_control = case_control,
            priors_col = priors_col,
            verbose = verbose,
            nThread = nThread,
            conda_env = conda_env)
        dat2 <- echodata::find_consensus_snps(
            dat = dat2,
            credset_thresh = credset_thresh,
            consensus_thresh = consensus_thresh,
            verbose = verbose)
        save_finemap_results(dat = dat2, 
                             path = file_path)
    }
    end_FM <- Sys.time()
    messager("+ Fine-mapping with",
             paste0("'",paste0(finemap_methods, collapse=", "),"'"),
             "completed:",v=verbose)
    round(difftime(end_FM,start_FM,units = "min"),2)
    return(dat2)
}
