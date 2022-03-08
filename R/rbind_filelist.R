#' Bind stored files
#'
#' Rapidly read a list of files from storage and concatenate them by rows.
#'
#' @family general
#' @keywords internal
#' @importFrom data.table fread rbindlist
#' @importFrom dplyr %>%
rbind_filelist <- function(file.list,
                           nThread=1,
                           verbose=TRUE){
    merged.dat <- parallel::mclapply(file.list, function(x){
        messager(x, v = verbose)
        dat <- data.table::fread(x)
        return(dat)
    }, mc.cores = nThread) %>% data.table::rbindlist(fill=TRUE)
    return(merged.dat)
}
