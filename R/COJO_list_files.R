COJO_list_files <- function(cojo_dir,
                            prefix,
                            nm_all = c("file.ma",
                                       "freq.badsnps",
                                       "excluded_snps.txt"),
                            ### stepwise only
                            nm_step = TRUE,
                            ### joint only ####
                            nm_joint = TRUE,
                            ### Conditional-only
                            nm_cond = TRUE
                            ){
    
    nm <- unique(
        c(
            nm_all,
            if(isTRUE(nm_step)) c("ldr.cojo","jma.cojo") else NULL,
            if(isTRUE(nm_joint)) c("ldr.cojo","jma.cojo") else NULL,
            if((!is.null(nm_cond)) && (!isFALSE(nm_cond)) ) {
                c("cma.cojo","cond.txt")
            } else {NULL}
        )  
    )
    paths <- as.list(
        file.path(cojo_dir,paste0(prefix,".",nm))
    )
    names(paths) <- gsub(paste0("^",prefix,c("\\.","\\-"),collapse = "|"),
                         "",basename(unlist(paths)))
    paths <- paths[names(paths) %in% nm]
    paths <- paths[unlist(lapply(paths, file.exists))]
    return(paths) 
}