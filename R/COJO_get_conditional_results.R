COJO_get_conditional_results <- function(cojo_dir = NULL,
                                         prefix = "cojo",
                                         cma_cojo_path =
                                             file.path(cojo_dir,
                                                       paste0(prefix,".cma.cojo")
                                                       ),
                                         cond_path =
                                             file.path(cojo_dir,
                                                       paste0(prefix,".cond.txt")
                                             ),
                                         pC_max = 0.05,
                                         verbose = TRUE
                                         ){
    bC <- NULL;
    
    res <- list()
    #### Read cma file ####
    if(!is.null(cma_cojo_path)){
        if(!file.exists(cma_cojo_path)){
            messager("cma_cojo_path does not exist. Returning NULL.",v=verbose) 
        } else {
            messager("Importing conditional results.",v=verbose)
            cma_res <- data.table::fread(cma_cojo_path)
            cma_res <- subset(cma_res, pC<=pC_max) |>
                dplyr::arrange(dplyr::desc(bC))
            res[["cma.cojo"]] <- cma_res
        }
    } 
    #### Read cond file ####
    if(!is.null(cond_path)){
        if(!file.exists(cond_path)){
            messager("cond_path does not exist. Returning NULL.",v=verbose) 
        } else if(readLines(cond_path)==""){
            messager("File is empty:",cond_path,v=verbose)
        } else {
            messager("Importing conditional results.",v=verbose)
            cond_res <- data.table::fread(cond_path, header=FALSE)$V1 
            colnames(cond_res)[2] <- paste0(colnames(cond_res)[2])
            res[["cond.txt"]] <- cond_res
        }  
    }   
    return(res)
}
