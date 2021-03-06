#' Get data 
#' 
#' Get data via \pkg{piggyback}.
#'
#' @importFrom piggyback pb_download
#' @keywords internal
get_data <- function(fname,
                     repo = "RajLabMSSM/echofinemap",
                     storage_dir = tempdir(),
                     overwrite = FALSE) {
    tmp <- file.path(storage_dir, fname)
    if (!file.exists(tmp)) {
        Sys.setenv("piggyback_cache_duration" = 10)
        dir.create(storage_dir, showWarnings = FALSE, recursive = TRUE)
        piggyback::pb_download(
            file = fname,
            dest = storage_dir,
            repo = repo,
            overwrite = overwrite
        )
    }
    return(tmp)
}
