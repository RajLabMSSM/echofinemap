#' Run full PAINTOR pipeline
#' 
#' @param dat A \link[data.table]{data.table} of GWAS/QTL/TWAS 
#' summary statistics. Alternatively, for multi-trait/multi-ancestry
#'  fine-mapping you can instead provide a named list of 
#'  \link[data.table]{data.table}s with overlapping genomic coordinates 
#'  (e.g. \code{list(GWAS1=dat1, GWAS2=dat2)}).
#' @source
#' \href{https://github.com/gkichaev/PAINTOR_V3.0}{GitHub}
#' \href{https://github.com/gkichaev/PAINTOR_V3.0/wiki/2a.-Computing-1000-genomes-LD}{LD Tutorial}
#' @export
#' @importFrom methods is
#' @examples 
#' dat <- echodata::BST1
#' LD_matrix <- echodata::BST1_LD_matrix
#' fullSS_path <- echodata::example_fullSS()
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' dat2 <- PAINTOR(dat = list("Nalls2019"=dat),
#'                 locus_dir = locus_dir)
PAINTOR <- function(dat,
                    annot = NULL,
                    dat_prefixes = NULL,
                    annot_prefixes = NULL,
                    zscore_col = "ZSCORE",
                    tstat_col = "tstat",
                    LD_matrix,
                    locus_dir,
                    fullSS_path, 
                    storage_dir = tempdir(),
                    paintor_path = NULL,
                    n_causal = 5,
                    XGR_dataset = NA,#"FANTOM5_Enhancer",
                    ROADMAP_search = NA,
                    chromatin_state = "TssA",
                    use_annotations = FALSE,
                    credset_thresh = .95,
                    consensus_thresh = 2,
                    multi_finemap_col_name = "PAINTOR",
                    trim_gene_limits = FALSE,
                    force_new_subset = FALSE,
                    dat_populations = "EUR",
                    LD_reference = "1KGphase3",
                    force_new_LD = FALSE,
                    force_reinstall = FALSE,
                    conda_env = "echoR_mini",
                    nThread = 1,
                    verbose = TRUE){ 
    # echoverseTemplate:::args2vars(PAINTOR)
    # echoverseTemplate:::source_all()
    
    # Note: All file formats are assumed to be single space delimited. 
    paintor_path <- PAINTOR_install(paintor_path = paintor_path,
                                    force_reinstall = force_reinstall, 
                                    verbose = verbose) 
    # dat[,ZSCORE:=(-log10(P))]
    # dat <- list(GWAS1=dat, GWAS2=dat)
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
    #### Check populations ####
    dat_populations <- PAINTOR_check_populations(
        dat_ls = dat_ls, 
        populations = dat_populations,
        LD_ls = LD_ls)
    ##### Report number of datasets/annotations and produce save path ####
    PT_results_path <- PAINTOR_datatype_handler(
        locus_dir = locus_dir, 
        dat_prefixes = names(dat_ls),
        annot_prefixes = names(annot_ls),
        verbose = verbose)  
    #### Merge data ####
    dat_merged <- PAINTOR_merge_datasets(dat_ls = dat_ls) 
    #### 2. LD Matrix File #### 
    LD_out <- PAINTOR_prepare_ld_multiancestry(
        locus_dir = locus_dir,
        dat_merged = dat_merged,
        LD_ls = LD_ls,
        PT_results_path = PT_results_path, 
        dat_populations = dat_populations,
        LD_reference = LD_reference,
        force_new_LD = force_new_LD, 
        nThread = nThread,
        verbose = verbose)
    #### Create Locus File while subsettin by match LD ####
    locusFile <- PAINTOR_create_locus_file(dat_merged = dat_merged,
                                           LD_ls = LD_ls,
                                           locus_dir = locus_dir,
                                           PT_results_path = PT_results_path)
    #### 3. Annotation Matrix File ####
    BED_paths <- PAINTOR_download_annotations(query_dat = dat_merged,
                                              locus_dir = locus_dir,
                                              PT_results_path = PT_results_path, 
                                              use_annotations = use_annotations,
                                              conda_env = conda_env,
                                              nThread = nThread,
                                              verbose = verbose)
    #### 4. Input File ####
    messager("+ PAINTOR:: Preparing input.files")
    data.table::fwrite(x = list(basename(locus_dir)),
                       file = file.path(PT_results_path, "input.files"),
                       sep="\n") 
    #### 5. Run PAINTOR! ####
    n_datasets <- sum(grepl(colnames(dat_merged), pattern = "^ZSCORE"))
    PAINTOR_run(paintor_path = paintor_path,
                PT_results_path = PT_results_path,
                n_datasets = n_datasets,
                #enumerate is actually faster when n_causal is small (<3)
                # but far larger n_causal use mcmc
                method = "mcmc",
                n_causal = n_causal,
                ld_paths = LD_out$paths)
    


  # Summarise the results
  # locus_name="LRRK2.Nalls23andMe_2019."; multi_finemap_col_name="PAINTOR"
  # locus_name="LRRK2.Nalls23andMe_2019.Fairfax_2014_CD14--Fairfax_2014_IFN--Fairfax_2014_LPS2--Fairfax_2014_LPS24"; multi_finemap_col_name="PAINTOR_Fairfax"

  PAINTOR_results <- PAINTOR_process_results(PT_results_path=PT_results_path,
                                             locus_name=locus_name)
  # Remove columns with the same name
  ff.cols <- grep(paste0(multi_finemap_col_name,"."), colnames(dat), value = TRUE)
  if(length(ff.cols)>0){dat[,c(ff.cols):=NULL]}
  # Merge results
  # LD_matrix <- readRDS("./Data/QTL/MESA/CAU/LRRK2/plink/LD_matrix.RData")
  dat.P <- PAINTOR_import_qtl_dat(QTL_datasets = QTL_datasets,
                                      locus = locus,
                                      trim_gene_limits = trim_gene_limits,
                                      force_new_subset = force_new_subset,
                                      metric = "P")
  dat.P <- echodata::merge_robust(dat.P,
                                subset(dat, select=c("SNP",
                                                            paste0(GWAS_datasets,".P"),
                                                            paste0(GWAS_datasets,".leadSNP")))
                                )

  merged_DT <- PAINTOR_merge_results(dat = dat.P,
                                     PAINTOR_results = PAINTOR_results,
                                     credset_thresh = credset_thresh,
                                     multi_finemap_col_name = multi_finemap_col_name)
  # merged_DT <- echodata::find_consensus_snps(merged_DT,
  #                                  credset_thresh = credset_thresh,
  #                                  consensus_thresh = consensus_thresh) 
  # PLOT
  transethnic_plot(merged_DT = merged_DT,
                   save_path = file.path(PT_results_path,"track_plot.enumerate2.png"),
                   PAINTOR_label="PAINTOR\nTrans-ethnic",
                   conditions = c("Nalls23andMe_2019","MESA_CAU","MESA_HIS"))
  return(merged_DT)
} 
