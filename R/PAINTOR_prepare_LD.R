#' Prepare LD file for PAINTOR
#'
#' \strong{VERY IMPORTANT!}\cr
#' "Particular care must be taken when computing LD from a reference panel 
#' such the 1000 genomes.
#' It is imperative that all the reference and alternate alleles for SNPs
#' from which the Z-scores were computed match the reference and 
#' alternate alleles of the reference panel.
#' The output of PAINTOR will not be correct if there are mismatches of
#' this type in the data.
#' Please see wiki section 2a for instructions on how to use the LD 
#' utility provided with the software."
#' @keywords internal
PAINTOR_prepare_LD <- function(LD_matrix,
                               dat_merged,
                               locus_dir,
                               PT_results_path,
                               verbose = TRUE){ 
    messager("++ PAINTOR:: Creating LD Matrix File.",v=verbose) 
    locus_name <- basename(locus_dir)
    ## Make sure SNPs are in the same order as the Locus File
    ld_path <- file.path(PT_results_path, paste0(locus_name,".ld1"))
    ld_dt <- echoLD::subset_common_snps(LD_matrix = LD_matrix,
                                        dat = dat_merged,
                                        verbose = verbose)
    #### Write ####
    messager("++ PAINTOR:: Writing LD file to ==> ",ld_path,v=verbose)
    data.table::fwrite(ld_dt$LD,
                       ld_path,
                       sep = " ", 
                       quote = FALSE,
                       col.names = FALSE, 
                       row.names = FALSE)
    return(ld_path)
}
