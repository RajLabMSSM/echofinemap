#' Check for necessary columns
#'
#' Check whether the input data (\code{dat}) 
#' has the minimum columns required
#' to run each fine-mapping method, as well as suggested columns.
#' @param dat \link[data.table]{data.table} containing SNP-level 
#' data to fine-map.  
#' @param verbose Print messages.
#' @inheritParams multifinemap
#' 
#' @export
#' @import cli
#' @importFrom data.table copy
#' @examples
#' dat <- echodata::BST1
#' finemap_methods <- check_required_cols(dat=dat)
check_required_cols <- function(dat,
                                finemap_methods=NULL, 
                                case_control=TRUE,
                                verbose=TRUE){
    noentry <- "\uD83D\uDEAB"
    checkmark <- "\u2705"
    warningsign <- "\u26A0"
    dat <- data.table::copy(dat) 
    #### Check requested methods #### 
    finemap_methods <- lfm(
        finemap_methods = finemap_methods,
        verbose = verbose)
    #### Get required cols table ####
    d <- required_cols(case_control = case_control,
                       verbose = FALSE) 
    #### Iterate through methods #####
    for(m in finemap_methods){
        if(verbose) cat(cli::bg_br_magenta(m)); cli::cat_line();
        #### Check required cols ####
        req <- d[m,]$required[[1]]
        req_missing <- req[!req %in% colnames(dat)]
        if(length(req_missing)>0){
            msg <- paste(
                noentry,"Missing required column(s) for",
                paste0(cli::col_br_magenta(m)," [skipping]",":"),
                cli::col_br_white(paste(req_missing,collapse=", "))
            )  
            #### Remove that method ####
            finemap_methods <- finemap_methods[finemap_methods != m]
            if(verbose) cat(cli::col_br_cyan(msg)); cli::cat_line();
            next()
        } else{
            msg <- paste(checkmark,"All required columns present.") 
            if(verbose) cat(cli::col_br_cyan(msg)); cli::cat_line();
        } 
        #### Check suggested cols ####
        sug <- d[m,]$suggested[[1]]
        if(length(sug)==0) next()
        sug_missing <- sug[!sug %in% colnames(dat)]
        if(length(sug_missing)>0){
            msg2 <- paste(
                cli::col_br_red(warningsign),
                "Missing optional column(s) for",
                paste0(cli::col_br_magenta(m),":"),
                cli::col_br_white(paste(sug_missing,collapse=", "))
            )
        } else {
            msg2 <- paste(checkmark,"All optional columns present.")
        }
        if(verbose) cat(cli::col_br_cyan(msg2)); cli::cat_line();
    }
    if(length(finemap_methods)==0) {
        stop("Your data does not have the sufficient columns to run",
             " any of the supported fine-mapping methods.",
             "\nSee echofinemap::required_cols() for details."
             )
    } 
    return(finemap_methods)
}
