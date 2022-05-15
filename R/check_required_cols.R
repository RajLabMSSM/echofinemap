#' Check for necessary columns
#'
#' Check whether the input data (\code{dat}) 
#' has the minimum columns required
#' to run each fine-mapping method, as well as suggested columns.
#' @param dat \link[data.table]{data.table} containing SNP-level 
#' data to fine-map.
#' @param finemap_methods Fine-mapping methods to check.
#' @param sample_size Manually-provided sample size 
#' (will be used is "N" column not present in \code{dat}). 
#' @param dataset_type Dataset type ("GWAS" or "QTL").
#' @param verbose Print messages.
#' 
#' @export
#' @examples
#' dat <- echodata::BST1
#' finemap_methods <- echofinemap::check_required_cols(dat=dat)
check_required_cols <- function(dat,
                                finemap_methods=NULL,
                                sample_size=NULL,
                                dataset_type="GWAS",
                                verbose=TRUE){
    noentry <- "\u26D4"
    checkmark <- "\u2705"
    warningsign <- "\u26A0"
    dat <- data.table::copy(dat)
    #### Add sample_size column ####
    if((!"N" %in% colnames(dat)) && (!is.null(sample_size) )){
        dat$N <- sample_size 
    }
    #### Check requested methods #### 
    finemap_methods <- list_finemap_methods(finemap_methods = finemap_methods,
                                            verbose = verbose)
    #### Get required cols table ####
    d <- required_cols(dataset_type = dataset_type) 
    #### Iterate through methods #####
    for(m in finemap_methods){
        message("vvvvv-- ",m)
        #### Check required cols ####
        req <- d[m,]$required[[1]]
        req_missing <- req[!req %in% colnames(dat)]
        if(length(req_missing)>0){
            message(noentry,"Missing required columns for ",m,": ",
                    paste(req_missing,collapse=", ")," (Skipping)");
            #### Remove that method ####
            finemap_methods <- finemap_methods[finemap_methods != m]
            next()
        } else{message(checkmark,"All required columns present.")}
        #### Check suggested cols ####
        sug <- d[m,]$suggested[[1]]
        if(length(sug)==0) next()
        sug_missing <- sug[!sug %in% colnames(dat)]
        if(length(sug_missing)>0){
            message(warningsign,"Missing optional columns for ",m,": ",
                    paste(sug_missing,collapse=", "))
        } else {
            message(checkmark,"All suggested columns present.")  
        }
    }
    if(length(finemap_methods)==0) {
        stop("Your data does not have the sufficient columns to run",
             " any of the supported fine-mapping methods.",
             "\nSee echofinemap::required_cols() for details."
             )
    } 
    return(finemap_methods)
}
