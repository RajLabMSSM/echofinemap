PAINTOR_get_suffixes <- function(dat_merged){
    suffixes <-  stringr::str_split(
        grep(colnames(dat_merged), pattern = "^ZSCORE",value = TRUE), 
        pattern = "\\.",
        simplify = TRUE
    )
    suffixes <- suffixes[,ncol(suffixes)]
    return(suffixes)
}