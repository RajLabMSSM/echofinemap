#' Create QTL locus file for PAINTOR
#'
#' @keywords internal
#' @importFrom data.table fwrite copy setnames
PAINTOR_create_locus_file <- function(dat_merged,
                                      LD_ls,
                                      locus_dir, 
                                      PT_results_path,
                                      NA_method=c("drop","fill")){
    
    NA_method <- tolower(NA_method)[1] 
    dat_merged <- data.table::copy(dat_merged)
    data.table::setnames(dat_merged,"SNP","RSID") 
    #### Save ####
    dir.create(PT_results_path,showWarnings = FALSE, recursive = TRUE)
    save_path <- file.path(PT_results_path, paste0(basename(locus_dir),
                                                   "locusFile.txt"))
    data.table::fwrite(x = dat_merged,
                       file = save_path,
                       sep=" ", 
                       quote = FALSE,
                       na = NA, 
                       nThread = 1)
    #### Return ####
    return(save_path)
}
