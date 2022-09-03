#' Gather all \emph{GCTA-COJO} results
#' 
#' @keywords internal
#' @family COJO 
#' @importFrom data.table := copy
COJO_process_results <- function(dat,
                                 paths,
                                 credset_thresh = 0.95,
                                 freq_cutoff = 0.1,
                                 verbose = TRUE){
    CS <- CS_cond <- SNP <- bJ <- bJ_se <- pJ <- LD_r <- 
        bC <- bC_se <- pC <- NULL;
    
    cojo_DT <- data.table::copy(dat)
    messager("+ COJO:: Processing results.") 
    #### Stepwise results #### 
    if("jma.cojo" %in% names(paths)){
        step_res <- COJO_get_stepwise_results(
            jma_cojo_path = paths$jma.cojo
        ) 
        if((!is.null(freq_cutoff)) && (!is.null(step_res))){
            step_res <- subset(step_res, freq_geno > freq_cutoff)
        }
        #### Merge with original data ####
        cojo_DT <- merge(x = cojo_DT,
                         y = step_res[,.(SNP,bJ,bJ_se,pJ,LD_r)],
                         by = "SNP",
                         all.x = TRUE)
        cojo_DT[,CS:=(ifelse(pJ<(1-credset_thresh),1,0))] 
    }  
    #### Conditional results #### 
    if("cma.cojo" %in% names(paths)){
        cond_res <- COJO_get_conditional_results(
            cma_cojo_path = paths$cma.cojo,
            cond_path = paths$cond.txt,
            verbose = verbose
        )
        cond_res <- cond_res$cma.cojo
        if((!is.null(freq_cutoff)) && (!is.null(cond_res))){ 
            cond_res <- subset(cond_res, freq_geno > freq_cutoff)
        }
        #### Merge with original data ####
        cojo_DT <- merge(x = cojo_DT,
                         y = cond_res[,.(SNP,bC,bC_se,pC)],
                         by = "SNP", 
                         all.x = TRUE) 
        cojo_DT[,CS_cond:=(ifelse(pC<(1-credset_thresh),1,0))] 
    }  
    return(cojo_DT)
}
