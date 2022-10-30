#' Get data 
#' 
#' Get data via \pkg{piggyback}.
#' @inheritParams echodata::get_data
#' @inheritDotParams echodata::get_data
#' @importFrom echodata get_data
#' @keywords internal
get_data <- function(fname,
                     repo = "RajLabMSSM/echofinemap",
                     save_dir = tempdir(),
                     tag = "latest",
                     overwrite = FALSE,
                     ...) {
    echodata::get_data(fname = fname, 
                       repo = repo, 
                       save_dir = save_dir, 
                       overwrite = overwrite,
                       tag = tag,
                       ...)
}
