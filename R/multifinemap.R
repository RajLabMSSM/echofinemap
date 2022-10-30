#' Multi-fine-map
#' 
#' Handle fine-mapping across multiple tools. 
#' @param fullSS_path Path to the full summary statistics file (GWAS or QTL)
#' that you want to fine-map.
#' It is usually best to provide the absolute path rather
#' than the relative path.
#' @param locus_dir Locus-specific directory to store results in.
#' @param dataset_type The kind dataset you're fine-mapping
#' (e.g. GWAS, eQTL, tQTL).
#' This will also be used when creating the subdirectory where your results
#' will be stored
#' (e.g. \emph{Data/<dataset_type>/Kunkle_2019}).
#' @param force_new_finemap By default, if an fine-mapping results file for
#'  a given locus is already present,
#' then \pkg{echolocatoR} will just use the preexisting file.
#' Set \code{force_new_finemap=T} to override this and re-run fine-mapping.
#' @param conditioned_snps Which SNPs to conditions on when fine-mapping
#' with (e.g. \emph{COJO}).
#' @param LD_matrix Linkage Disequilibrium (LD) matrix to use for fine-mapping.
#' @param LD_reference Name of the LD reference panel.
#' @param finemap_methods Fine-mapping methods to run. 
#' See \link[echofinemap]{lfm} for a list of all fine-mapping methods currently
#' available.
#' @param finemap_args A named nested list containing additional arguments 
#' for each fine-mapping method. e.g.
#' \code{finemap_args = list(FINEMAP=list(), PAINTOR=list(method=""))}
#' @param n_causal The maximum number of potential causal SNPs per locus.
#' This parameter is used somewhat differently by different fine-mapping tools.
#' See tool-specific functions for details.
#' @param priors_col [Optional] Name of the a column in 
#' \code{dat} to extract SNP-wise prior probabilities from.
#' @param case_control Whether the summary statistics come from a case-control
#' study (e.g. a GWAS of having Alzheimer's Disease or not) (\code{TRUE})
#' or a quantitative study (e.g. a GWAS of height, or an eQTL) (\code{FALSE}).
#' @param seed Set the random seed for reproducible results.
#' @param nThread Number of threads to parallelise across (when applicable).
#' @param conda_env Conda environment to use.
#' @param verbose Print messages.
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
    #### Check methods ####
    finemap_methods <- lfm(finemap_methods = finemap_methods,
                           verbose = verbose)
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
            case_control = case_control,
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
            credset_thresh = credset_thresh,
            case_control = case_control,
            priors_col = priors_col,
            seed = seed,
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
