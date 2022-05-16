FINEMAP_check_version_results <- function(finemap_version,
                                          results_file){
    if((finemap_version < "1.3.1") &
       any((results_file==".cred"))){
        messager(
            "WARNING: FINEMAP<1.3.1 does not produce .cred results files.\n",
            "Using marginal probabilties from .snp results file instead.")
        results_file <- ".snp"
    }
    return(results_file)
}