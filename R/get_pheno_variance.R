#' Compute phenotype variance
#' 
#' Compute phenotype variance (\code{var_y} in \pkg{susieR}).
#' @inheritParams multifinemap
#' @inheritParams SUSIE
#' @keywords internal
#' @importFrom stats var
get_pheno_variance <- function(dat, 
                               case_control,
                               var_y,
                               verbose=TRUE){
    
    #### From user-supplied numeric vector ####
    if(is.numeric(var_y)){
        messager("++ Computing phenotypic variance",
                 "from user-supplied numeric vector.",v=verbose)
        pheno_var <- stats::var(var_y, na.rm = TRUE)
        
    #### from data ####
    } else if(is.character(var_y)){ 
        if(var_y %in% colnames(dat)){
            pheno_var <- stats::var(dat[[var_y]]) 
           
        } else if(isTRUE(case_control) & 
                  "N_cases" %in% colnames(dat) & 
                  "N_controls" %in% colnames(dat)){
            messager("++ Computing phenotype variance.",v=verbose)
            pheno_var <- stats::var(
                c(rep(0, max(dat$N_cases, na.rm = TRUE)),
                  rep(1, max(dat$N_controls, na.rm = TRUE))) 
                ) 
        } else {
            messager(
                "Phenotype variance could not be calculated from this data.",
                v=verbose)
            messager("Estimating prior variance instead.",v=verbose)
            pheno_var <- 1
        } 
    } 
    return(pheno_var)
}
