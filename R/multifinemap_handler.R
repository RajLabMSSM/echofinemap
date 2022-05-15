#' Fine-map using multiple fine-mapping tools
#'
#' Fine-mapping will be repeated on the same locus using each of the tools 
#' in \code{finemap_methods}.
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
multifinemap_handler <- function(dat,
                                 locus_dir,
                                 fullSS_path=NULL,
                                 finemap_methods,
                                 finemap_args=NULL,
                                 dataset_type,
                                 force_new_finemap=FALSE,
                                 LD_matrix=NULL,
                                 n_causal=5,
                                 sample_size=NULL,
                                 conditioned_snps=NULL,
                                 PAINTOR_QTL_datasets=NULL,
                                 PP_threshold=.95,
                                 case_control=TRUE,
                                 verbose=TRUE,
                                 nThread=1,
                                 conda_env="echoR_mini"){
  # PAINTOR_QTL_datasets=NULL;PP_threshold=.95; effect_col="Effect";
  #n_causal=5; sample_size=1000; stderr_col="StdErr"; pval_col="P"; 
  #N_cases_col="N_cases"; N_controls_col="N_controls"; A1_col="A1"; 
  #A2_col="A2";conditioned_snps=NULL;
  
  messager("++ Fine-mapping using",length(finemap_methods),"tool(s):",
           paste(finemap_methods, collapse=", "),v=verbose)
  #### Subset dat/LD ####
  sub.out <- echoLD::subset_common_snps(LD_matrix = LD_matrix,
                                        dat = data.table::copy(dat),
                                        verbose  = FALSE)
  LD_matrix <- sub.out$LD
  dat2 <- sub.out$DT  
  #### Iterate over methods #####
  for(i in seq_len(length(unique(finemap_methods)))){
    m <- unique(finemap_methods)[i];
    message("\n+++ Multi-finemap:: ",m," +++")
    d <- tryCatch({ 
      multifinemap_handler_method(
         dat = dat2,
         fullSS_path = fullSS_path,
         locus_dir = locus_dir,
         finemap_method = m,
         finemap_args=finemap_args, 
         force_new_finemap = force_new_finemap,
         dataset_type = dataset_type,
         LD_matrix = LD_matrix,
         n_causal = n_causal,
         sample_size = sample_size,
         PAINTOR_QTL_datasets = PAINTOR_QTL_datasets,
         PP_threshold = PP_threshold,
         case_control = case_control,
         conditioned_snps = conditioned_snps,
         verbose = verbose,
         nThread=nThread,
         conda_env = conda_env)
     }, error = function(e) {print(e); NULL}) 
    #### If fine-mapping worked, merge the results back into the data ####
    if(!is.null(d) && 
        nrow(d)>0 && 
        all(c("SNP","CS","PP") %in% colnames(d)) ){
        messager("++ Credible Set SNPs identified =",
                 nrow(subset(d, CS>0)),v=verbose)
        #### Add results to method-specific columns ####
        messager("++ Merging",m,"results with multi-finemap data.",v=verbose);
        value_var <- if(m=="COJO"){"Conditioned_Effect"}else{"PP"};
        d <- subset(d, select = c("SNP","CS",value_var) );
        ####  Rename columns according to method name ####
        cols <- colnames(d);
        colnames(d) <- c("SNP", paste(m, cols[seq(2,length(cols))], sep="." ));
        ##### Merge new columns into DT ####
        dat2 <- drop_finemap_cols(dat = dat2,
                                  finemap_methods = m, 
                                  verbose = FALSE)
        
        ####--------------------------------------------------####
        #### Something wacky is going on here with data.table ####
        ## Only SUSIE results are merging, the rest are NAs, unless you set 
        ## all.y=TRUE, in which case duplicate SNPs/row are generated.
        ## Even setting the keys doesn't seem to help.
        ## 
        ## The only way to fix this seems to be converting to data.frames 
        ## temporarily, merge, and then convert back to data.table. 
        data.table::setkey(dat2,SNP)
        data.table::setkey(d,SNP)
        # preview_data(str(d))
        # preview_data(str(dat2))
        # message("========\n")
        # preview_data( dat2[d,])
        dat2 <- merge(
          x = data.frame(dat2, check.names = FALSE),
          y = data.frame(d, check.names = FALSE), 
          all.x = TRUE);
        dat2 <- data.table::data.table(dat2)
        ####--------------------------------------------------####
      } 
  }
  if(nrow(dat2)!=dplyr::n_distinct(dat2$SNP)) {
    warning("Duplicate SNP rows detected.")
  }
  return(dat2)
}
