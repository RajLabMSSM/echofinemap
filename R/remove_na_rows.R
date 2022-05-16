#' Remove NA rows
#' 
#' Remove rows with NAs in one or more specific columns.
#' If the column is not present, it will not be used for filtering. 
#' 
#' @param dat Data
#' @param cols Column names.
#' @param verbose Print messages.
#' @keywords internal
#' @returns Filtered data
#' @importFrom dplyr filter_at
remove_na_rows <- function(dat, 
                           cols=c("Effect","P",
                                       "StdErr","SNP",
                                       "MAF"),
                           verbose=TRUE){
    cols <- cols[cols %in% colnames(dat)]
    og_rows <- nrow(dat)
    dat <- dplyr::filter_at(.tbl = dat, 
                            .vars = cols, 
                            .vars_predicate = function(x){!is.na(x)})
    if(nrow(dat)!=og_rows){
        messager(og_rows - nrow(dat),
                 "rows were removed due to NAs",
                 "in one or more utilized columns.",v=verbose)
    }
    return(dat)
}
