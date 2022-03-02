#' Compute phenotype variance
#' 
#' Compute phenotype variance (\code{var_y} in \pkg{susieR}).
#' 
#' @keywords internal
#' @importFrom stats var
get_pheno_variance <- function(dat,
                               dataset_type,
                               var_name="Expression",
                               verbose=TRUE){ 
    if(toupper(dataset_type)=="GWAS" & 
       "N_cases" %in% colnames(dat) & 
       "N_controls" %in% colnames(dat)){
        messager("++ Computing phenotype variance.",v=verbose)
        phenotype_variance <- stats::var(
            c(rep(0, max(dat$N_cases, na.rm = TRUE)),
              rep(1, max(dat$N_controls, na.rm = TRUE)))
        )
    } else if(endsWith(toupper(dataset_type),"QTL")){
        if(var_name %in% colnames(dat)){
            stop("Cannot find column specified by var_name=",
                 paste0("'",var_name,"'"))
        }
        phenotype_variance <- stats::var(dat[[var_name]])
    } else {
        messager(
            "Phenotype variance could not be calculated from this data.",
            v=verbose)
        messager("Estimating prior variance instead.",v=verbose)
        phenotype_variance <- 1
    }
    return(phenotype_variance)
}
