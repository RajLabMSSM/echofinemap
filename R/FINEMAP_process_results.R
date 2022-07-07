#' FINEMAP: process results
#' 
#' Post-processing of \code{FINEMAP} results.
#'
#' @family FINEMAP
#' @keywords internal
#' @source \url{http://www.christianbenner.com}
#' @importFrom dplyr %>% arrange desc
#' @source 
#' \code{
#' locus_dir <- file.path(tempdir(), echodata::locus_dir)
#' dat <- echodata::BST1;
#' dat2 <- echofinemap:::FINEMAP_process_results(dat=dat, locus_dir=locus_dir)
#' }
FINEMAP_process_results <- function(dat,
                                    locus_dir,
                                    credset_thresh=.95,
                                    pvalue_thresh=.05, 
                                    finemap_version=package_version("1.4.1"),
                                    file_options=NULL,
                                    sort_by_CS=TRUE,
                                    verbose=TRUE){
    PP <- rsid <- prob <- NULL;
    #### Notes on FINEMAP output files ####
    ##
    ## .snp and .cred are often similar, but not identical.
    ## Recommendation: use the .cred file that shows the largest posterior 
    ## probability for the number of causal variants in line 1 of the file.
    ## and extract credible sets from that file. 
    pp_cols <- c("PP","PP_snp","PP_config","CS","k") 
    pp_cols <- pp_cols[pp_cols %in% names(dat)]
    if(length(pp_cols)>0) dat <- dat[,-pp_cols, with=FALSE]
    
    #### Double check which results files are available ####
    ## This vary depending on which version of FINEMAP you're using.
    file_options <- FINEMAP_check_files(locus_dir = locus_dir, 
                                        file_options = file_options) 
    #### Add conditional probabilities ####
    if(".cred" %in% file_options){
        data.cred <- FINEMAP_import_data_cred(
            locus_dir = locus_dir, 
            credset_thresh = credset_thresh, 
            pvalue_thresh = pvalue_thresh,
            finemap_version = finemap_version,
            verbose = verbose)  
       dat <- data.table::merge.data.table(
           data.table::data.table(dat),
           data.table::data.table(data.cred),
           by="SNP",
           all.x = TRUE) 
    } 
    #### Add marginal probabilities ####
    if (".snp" %in% file_options){
        data.snp <- FINEMAP_import_data_snp(
            credset_thresh = credset_thresh,
            locus_dir = locus_dir,
            verbose = verbose)  
        dat <- data.table::merge.data.table(
            data.table::data.table(dat),
            data.table::data.table(
                dplyr::select(data.snp,
                              SNP=rsid,
                              PP_snp=prob)),
            by = "SNP", 
            all.x = TRUE) 
    } 
    #### Add configuration probabilities ####
    if (".config" %in% file_options){
        data.config <- FINEMAP_import_data_config(
            locus_dir = locus_dir,
            credset_thresh = credset_thresh,
            pvalue_thresh = pvalue_thresh,
            finemap_version = finemap_version,
            verbose = verbose)  
        if(nrow(data.config)>0){
            dat <- data.table::merge.data.table(
                data.table::data.table(dat),
                data.table::data.table(data.config) %>%
                    dplyr::rename(PP_config=prob),
                by="SNP",
                all.x = TRUE) 
        }
    }
    #### Sort so that CS SNPs are at the top ####
    if(sort_by_CS & ("CS" %in% colnames(dat))){
        dat <- dplyr::arrange(dat, dplyr::desc(PP))
    }
    #### Ensure there's no duplicate rows ####
    dup_snps <- sum(duplicated(dat$SNP), na.rm = TRUE)
    if(dup_snps>0) {
        stp <- paste(dup_snps,"duplicated SNPs found across rows.")
        stop(stp)
    }
    return(dat)
}

