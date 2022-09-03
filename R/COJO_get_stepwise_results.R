#' Gather stepwise conditional results
#'
#' Gather and preprocess the results of the \emph{GCTA-COJO} 
#' conditional stepwise procedure.
#' \href{https://yanglab.westlake.edu.cn/software/gcta}{
#' \strong{GTCA-COJO Documentation}:}\cr
#' "Perform a stepwise model selection procedure to select 
#' independently associated SNPs. Results will be saved in a 
#' \emph{.jma} file with additional file \emph{.jma.ldr} showing the LD 
#' correlations between the SNPs."
#' @family COJO
#' \url{https://www.nature.com/articles/ng.2213}
#' \url{https://www.cell.com/ajhg/fulltext/S0002-9297(10)00598-7}
#' \url{https://cnsgenomics.com/software/gcta/#Overview}
#' @keywords internal
#' @inheritParams COJO
#' @returns independent_snps
COJO_get_stepwise_results <- function(cojo_dir = NULL,
                                      prefix = "cojo",
                                      jma_cojo_path = 
                                          file.path(cojo_dir,
                                                    paste0(prefix,".jma.cojo")
                                                    ),
                                      verbose = TRUE){ 
    if(is.null(jma_cojo_path)){
        return(NULL)
    } else if(!file.exists(jma_cojo_path)) {
        messager("jma_cojo_path does not exist. Returning NULL.",v=verbose) 
    } else {
        messager("Importing stepwise results.",v=verbose)
        independent_snps <- data.table::fread(jma_cojo_path)
        independent_snps$LD_r2 <- independent_snps$LD_r^2
        return(independent_snps)
    } 
}
