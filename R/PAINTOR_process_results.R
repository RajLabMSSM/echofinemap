#' PAINTOR: process results
#'
#' Process PAINTOR fine-mapping results.
#' @param dat_merged Merged PAINTOR results.
#' @param res_paths Paths to PAINTOR results.
#' @inheritParams echodata::find_consensus_snps
#' 
#' @keywords internal
#' @importFrom data.table rbindlist fread
#' @importFrom dplyr rename
#' @importFrom stats setNames
PAINTOR_process_results <- function(dat_merged,
                                    res_paths,
                                    credset_thresh,
                                    verbose=TRUE){
    
    Posterior_Prob <- RSID <- PP <- NULL; 
    
    messager("+ PAINTOR:: Processing results.",v=verbose)
    res <- lapply(stats::setNames(names(res_paths$RESname),
                                  names(res_paths$RESname)),
                  function(nm){ 
        d <- data.table::fread(res_paths$RESname[[nm]],
                               key = "RSID") |>
            dplyr::select(RSID,PP=Posterior_Prob)
        d[,CS:=(ifelse(PP>=credset_thresh,1,0))]
        if(length(res_paths$RESname)>1){
            data.table::setnames(d,c("PP","CS"),
                                 paste(c("PP","CS"),nm,sep="_"))
        }
        # res <- cbind(res, d[dat_merged$SNP,.(PP,CS)])
        return(d)
    }) |> Reduce(f = function(...){merge(...,by="RSID")} ) |>
        merge(x = dat_merged, by.x = "SNP", by.y = "RSID") 
    return(res)
}
