#' Create QTL locus file for PAINTOR
#'
#' @keywords internal
#' @importFrom data.table fwrite copy setnames
PAINTOR_create_locus_file <- function(dat_merged,
                                      shared_snps,
                                      locus_dir, 
                                      PT_results_path,
                                      NA_method=c("drop","fill")){
    
    NA_method <- tolower(NA_method)[1] 
    dat_merged <- data.table::copy(dat_merged)
    data.table::setnames(dat_merged,"SNP","RSID") 
    data.table::setkeyv(dat_merged,"RSID")
    #### Subset and reorder to align with LD matrices ####
    dat_merged <- dat_merged[shared_snps,] 
    #### Save ####
    dir.create(PT_results_path,showWarnings = FALSE, recursive = TRUE)
    ### Can't have any suffixes, literally just the locus name ####
    save_path <- file.path(PT_results_path, basename(locus_dir))
    data.table::fwrite(x = dat_merged,
                       file = save_path,
                       sep=" ", 
                       quote = FALSE,
                       na = NA, 
                       nThread = 1)
    #### Return ####
    return(save_path)
}
