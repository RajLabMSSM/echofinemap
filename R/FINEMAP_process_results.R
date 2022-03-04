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
                                    finemap_version="1.4",
                                    results_file=".cred",
                                    nThread=1,
                                    sort_by_CS=TRUE,
                                    verbose=TRUE){
    PP <- NULL;
    #### Notes on FINEMAP output files ####
    ##
    ## .snp and .cred are often similar, but not identical.
    ## Recommendation: use the .cred file that shows the largest posterior 
    ## probability for the number of causal variants in line 1 of the file.
    ## and extract credible sets from that file. 
    
    #### Handling FINEMAP version differences  ####
    results_file <- FINEMAP_check_version_results(
        finemap_version = finemap_version,
        results_file = results_file)
    #### Double check which results files are available ####
    ## This vary depending on which version of FINEMAP you're using.
    results_file <- FINEMAP_check_files(locus_dir, results_file)
    #### Process FINEMAP results ####
    if(results_file==".cred"){
        data.cred <- FINEMAP_import_data_cred(locus_dir = locus_dir,
                                              verbose = verbose) 
        dat2 <- data.table::merge.data.table(data.table::data.table(dat),
                                            data.table::data.table(data.cred),
                                            by="SNP",
                                            all.x = TRUE)
    } else if (results_file==".snp"){
        data.snp <- FINEMAP_import_data_snp(locus_dir = locus_dir,
                                            verbose = verbose) 
        dat2 <- data.table::merge.data.table(data.table::data.table(dat),
                                            data.table::data.table(
                                                subset(data.snp, 
                                                       select=c("rsid",
                                                                "PP",
                                                                "CS")) ),
                                            by.x = "SNP",
                                            by.y="rsid",
                                            all.x = TRUE)
    } else if (results_file==".config"){
        data.config <- FINEMAP_import_data_config(locus_dir = locus_dir,
                                                  verbose = verbose)
        dat2 <- data.table::merge.data.table(data.table::data.table(dat),
                                            data.table::data.table(data.config),
                                            by="SNP",
                                            all.x = TRUE)
    }
    #### Sort so that CS SNPs are at the top ####
    if(sort_by_CS){
        dat2 <- dat2 %>% dplyr::arrange(dplyr::desc(PP))
    }
    return(dat2)
}
