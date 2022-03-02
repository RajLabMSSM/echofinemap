#' Fine-map using multiple fine-mapping tools
#'
#' Fine-mapping will be repeated on the same locus using each of the tools 
#' in \code{finemap_method_list}.
#' Then, all results will be merged into the locus-specific multi-finemap file,
#' along with the original per-SNP GWAS/QTL summary statistics.
#' Each tools will have the following columns:
#' \describe{
#'  \item{<tool>.PP}{The posterior probability (PP) of a SNP being causal for
#'   the trait. Though this is a generalization and the exact meaning of PP
#'    will differ by tools (e.g. Posterior Inclusion Probability for SUSIE).}
#'  \item{<tool>.CS}{Which credible set the SNP is part of (within a locus). 
#'  If \code{=0}, then the SNP was not part of any credible set. 
#'  Some tools only produce one credible set per locus.}
#' }
#'
#' @family finemapping functions
#' @keywords internal
#' @examples
#' \dontrun{
#' BST1 <- echodata::BST1; data("BST1_LD_matrix");
#' dat <- BST1
#' finemap_method_list <- c("ABF","SUSIE")
#' }
multi_finemap <- function(locus_dir,
                          fullSS_path,
                          finemap_method_list,
                          finemap_args=NULL,
                          dat,
                          dataset_type,
                          LD_matrix=NULL,
                          n_causal=5,
                          sample_size=NULL,
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
                          case_control=TRUE,
                          verbose=TRUE,
                          nThread=1,
                          conda_env="echoR"){
  # PAINTOR_QTL_datasets=NULL;PP_threshold=.95; effect_col="Effect"; n_causal=5; sample_size=1000; stderr_col="StdErr"; pval_col="P"; N_cases_col="N_cases"; N_controls_col="N_controls"; A1_col="A1"; A2_col="A2";conditioned_snps=NULL;
  messager("++ Fine-mapping using multiple tools:",
           paste(finemap_method_list, collapse=", "),v=verbose)
  # Check overlap
  sub.out <- echoLD::subset_common_snps(LD_matrix = LD_matrix,
                                        dat = dat,
                                        verbose  = FALSE)
  LD_matrix <- sub.out$LD
  dat <- sub.out$DT

  select_cols <- colnames(dat)[
    !grepl(colnames(dat),
           pattern = paste(c(finemap_method_list,
                             "Support","Consensus_SNP"),collapse = "|"))]
  merged_dat <- subset(dat, select = select_cols)

  for(i in seq_len(length(unique(finemap_method_list)))){
    m <- unique(finemap_method_list)[i];
    message("\n+++ Multi-finemap:: ",m," +++")
    finemap_dat <- null_DT <- data.table::data.table(SNP=merged_dat$SNP,
                                                     CS=NA,
                                                     PP=NA);
    # DT <- tryCatch({
      # EXPRESSION
     try({
       finemap_dat <- finemap_method_handler(
         fullSS_path = fullSS_path,
         locus_dir = locus_dir,
         finemap_method = m,
         finemap_args=finemap_args,
         dat = data.table::as.data.table(
           dat
         ),
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
         conditioned_snps = conditioned_snps,
         verbose = verbose,
         nThread=nThread,
         conda_env = conda_env)
     })
      # },
      # # WARNING
      # warning = function(w){messager("WARNING")},
      # # ERROR
      # error = function(e){
      #   message("SUSIE::Error:: Could not identify Credible Set.");
      #   return(null_DT)
      #   }
      # ) ## End tryCatch
      try({messager("++ Credible Set SNPs identified =",
                    nrow(subset(finemap_dat, CS>0)),v=verbose )})
      # Add results to method-specific columns
      messager("++ Merging",m,"results with multi-finemap data.",v=verbose);
      value_var <- if(m=="COJO"){"Conditioned_Effect"}else{"PP"};
      dat_select <- subset(finemap_dat, select = c("SNP","CS",value_var) );
      # Rename columns according to method name
      cols <- colnames(dat_select);
      colnames(dat_select) <- c("SNP", 
                                paste(m, cols[seq(2,length(cols))], sep="." ));
      # Merge new columns into DT
      sum(data.table::as.data.table(merged_dat)$SNP %in%
            data.table::as.data.table(dat_select)$SNP)
      merged_dat <- data.table::merge.data.table(x = merged_dat,
                                                  y = dat_select,
                                                  by = "SNP",
                                                  all.x = TRUE);
  }
  if(nrow(merged_dat)!=dplyr::n_distinct(merged_dat$SNP)) {
    stop("Duplicate SNP rows detected.")
  }
  return(merged_dat)
}

 



