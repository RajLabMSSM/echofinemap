#' Merge PAINTOR results
#'
#'  @keywords internal
PAINTOR_merge_results <- function(dat,
                                  PAINTOR_results,
                                  credset_thresh=.5,
                                  multi_finemap_col_name="PAINTOR"){
    messager("PAINTOR:: Merging PAINTOR results with multi-finemap file.")
    merged_DT <- echodata::merge_robust(dat, PAINTOR_results[,c("RSID","Posterior_Prob")],
                                        by.x="SNP", by.y="RSID", all.x = TRUE)
    PP.col.name <- paste0(multi_finemap_col_name,".PP")
    names(merged_DT)[names(merged_DT) == "Posterior_Prob"] <- PP.col.name
    merged_DT[,paste0(multi_finemap_col_name,".CS")] <- ifelse(subset(merged_DT,select=PP.col.name) > credset_thresh, 1, 0)
    messager("PAINTOR:: Credible Set size =",sum(subset(merged_DT, select=paste0(multi_finemap_col_name,".CS")),na.rm=TRUE))
    return(merged_DT)
}
