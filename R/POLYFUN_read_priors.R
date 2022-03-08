#' Read priors
#' 
#' Read PolyFun prior probabilities.
#'  Handles PolyFun version differences in file formatting
#'   (see \href{https://github.com/RajLabMSSM/echolocatoR/issues/80}{here}).
#' @keywords internal 
#' @importFrom dplyr %>%
#' @importFrom data.table fread
POLYFUN_read_priors <- function(snp_w_priors.file,
                                nThread=1,
                                verbose=TRUE){
    SNP_x <- SNP_y <- NULL;
    
    #### Import results #### 
    priors <- data.table::fread(snp_w_priors.file,
                                nThread = nThread)
    if(nrow(priors)==0){
        messager("No priors identified. Returning empty data.table.")
        return(priors)
    } else {
        messager("++ Importing precomputed priors.",v=verbose) 
        if("SNP_x" %in% colnames(priors)){
            priors <- priors %>% dplyr::rename(SNP=SNP_x)
        }
        if("SNP_y" %in% colnames(priors)){
            priors <- priors %>% dplyr::select(-SNP_y)
        }
        return(priors)
    } 
}
