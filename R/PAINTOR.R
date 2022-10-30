#' PAINTOR: fine-map
#' 
#' Run the full PAINTOR fine-mapping pipeline.
#' @source
#' \href{https://github.com/gkichaev/PAINTOR_V3.0}{GitHub}
#' \href{https://github.com/gkichaev/PAINTOR_V3.0/wiki/2a.-Computing-1000-genomes-LD}{LD Tutorial}
#' \href{https://github.com/gkichaev/PAINTOR_V3.0/wiki/2.-Input-Files-and-Formats}{Input file formats}
#' @param method \code{"enumerate"} is actually faster 
#' when \code{max_causal} is small (<3), 
#' but far larger \code{max_causal} use \code{"mcmc"}.
#' @param zscore_col Name of the column containing the normalized Z-statistic.
#' @param tstat_col [Optional] Name of the column containing the t-statistic.
#' @param max_causal Maximum number of causal SNPs.
#' @param use_annotations Whether to perform functional fine-mapping with 
#' specified annotations (\code{TRUE}) 
#' or simply perform statistical fine-mapping without any annotations.
#' @param annot Custom annotations to use for functional fine-mapping.
#' @param annot_xgr Use annotations from \pkg{XGR}
#' via \link[echoannot]{XGR_query}.
#' @param annot_roadmap Use annotations from \pkg{Roadmap} 
#' via \link[echoannot]{ROADMAP_query}.
#' @param force_reinstall Force reinstallation of PAINTOR.
#' @param superpopulation The ancestry of each population in \code{dat}. 
#' Can be one (applied to all dataset) or more (one per dataset).
#' When users don't supply their own Linkage Disequilibrium (LD) 
#' matrix/matrices via the \code{LD_matrix} argument, 
#' this information is used to gather LD matrices 
#' from the reference panel(s) using \link[echoLD]{get_LD}.
#' @param paintor_path [Optional] Path to PAINTOR executable. 
#' Will be automatically installed if set to \code{NULL} (default). 
#' @inheritParams multifinemap
#' @inheritParams echodata::find_consensus_snps
#' @inheritParams echoannot::ROADMAP_query
#' @inheritParams echoLD::get_LD
#' 
#' @export
#' @importFrom methods is
#' @examples  
#' dat <- echodata::BST1
#' ## For example only; 
#' ## normally you need to compute ZSCORE using the 
#' ## full genome-wide summary stats.
#' dat[,ZSCORE:=(-log10(P))]
#' LD_matrix <- echodata::BST1_LD_matrix  
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' dat2 <- PAINTOR(dat = dat,
#'                 locus_dir = locus_dir,
#'                 LD_matrix = LD_matrix,
#'                 max_causal = 2,
#'                 method = "enumerate") 
PAINTOR <- function(dat,
                    LD_matrix,
                    locus_dir, 
                    annot = NULL,
                    zscore_col = "ZSCORE",
                    tstat_col = "tstat", 
                    max_causal = 1,
                    use_annotations = FALSE,
                    annot_xgr = NULL,
                    annot_roadmap = NULL,
                    chrom_states = NULL, 
                    credset_thresh = .95, 
                    superpopulation = "EUR",
                    LD_reference = "1KGphase3",
                    force_new_LD = FALSE, 
                    method = c("mcmc","enumerate"),
                    seed = 2022,
                    paintor_path = NULL,
                    force_reinstall = FALSE,
                    conda_env = "echoR_mini",
                    nThread = 1,
                    verbose = TRUE){ 
    # echoverseTemplate:::args2vars(PAINTOR)
    # echoverseTemplate:::source_all()
    
    if(is.null(method)){
        method <- eval(formals(PAINTOR)$method)[1]
        messager("method=NULL, setting default method:",method,v=verbose)
    }
    # Note: All file formats are assumed to be single space delimited. 
    paintor_path <- PAINTOR_install(paintor_path = paintor_path,
                                    force_reinstall = force_reinstall, 
                                    verbose = verbose)  
    dat_ls <- PAINTOR_check_required_cols(dat = dat, 
                                          zscore_col = zscore_col, 
                                          tstat_col = tstat_col)
    annot_ls <- PAINTOR_check_required_cols(dat = annot, 
                                            zscore_col = zscore_col, 
                                            tstat_col = tstat_col) 
    #### Check LD ####
    LD_ls <- PAINTOR_check_ld(LD_matrix = LD_matrix, 
                              dat_ls = dat_ls,
                              locus_dir = locus_dir,
                              verbose = verbose)
    #### Check superpopulation ####
    superpopulation <- PAINTOR_check_superpopulation(
        dat_ls = dat_ls, 
        superpopulation = superpopulation,
        LD_ls = LD_ls)
    ##### Report number of datasets/annotations and produce save path ####
    PT_results_path <- PAINTOR_datatype_handler(
        locus_dir = locus_dir, 
        dat_prefixes = names(dat_ls),
        annot_prefixes = names(annot_ls),
        verbose = verbose)  
    #### Merge data ####
    dat_merged <- PAINTOR_merge_datasets(dat_ls = dat_ls, 
                                         verbose = verbose) 
    #### 2. LD Matrix File #### 
    LD_out <- PAINTOR_prepare_ld_multiancestry(
        locus_dir = locus_dir,
        dat_merged = dat_merged, 
        LD_ls = LD_ls,
        PT_results_path = PT_results_path, 
        superpopulation = superpopulation,
        LD_reference = LD_reference,
        force_new_LD = force_new_LD, 
        nThread = nThread,
        verbose = verbose)
    dat_merged <- LD_out$dat_merged
    LD_ls <- LD_out$LD_ls
    ld_paths <- LD_out$ld_paths
    #### Create Locus File while subsetting by matched LD ####
    locusFile_path <- PAINTOR_create_locus_file(
        dat_merged = dat_merged, 
        locus_dir = locus_dir,
        PT_results_path = PT_results_path,
        verbose = verbose)
    #### 3. Annotation Matrix File ####
    ANname <- PAINTOR_download_annotations(dat_merged = dat_merged, 
                                           locus_dir = locus_dir,
                                           PT_results_path = PT_results_path, 
                                           use_annotations = use_annotations,
                                           annot_xgr = annot_xgr,
                                           annot_roadmap = annot_roadmap,
                                           chrom_states = chrom_states,
                                           conda_env = conda_env,
                                           nThread = nThread,
                                           verbose = verbose)
    #### 4. Input File ####
    inputFile_path <- PAINTOR_input_file(locus_dir = locus_dir, 
                                         PT_results_path = PT_results_path, 
                                         verbose = verbose)
    #### 5. Run PAINTOR! ####
    zscore_cols <- grep(colnames(dat_merged), pattern = "^ZSCORE",
                        value = TRUE) 
    res_paths <- PAINTOR_run(paintor_path = paintor_path,
                             PT_results_path = PT_results_path,
                             inputFile_path = inputFile_path, 
                             zscore_cols = zscore_cols, 
                             method = method, 
                             max_causal = max_causal,
                             seed = seed,
                             ld_paths = ld_paths,
                             verbose = verbose) 
    res_paths[["locusFile"]] <- locusFile_path
    #### 6. Gather results ####
    dat_merged <- PAINTOR_process_results(dat_merged = dat_merged,
                                          res_paths = res_paths,
                                          credset_thresh = credset_thresh,
                                          verbose = verbose)     
  return(dat_merged)
} 
