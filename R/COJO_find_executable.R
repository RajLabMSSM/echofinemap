#' COJO: find executable
#' 
#' Find the path to the COJO executable.
#' @inheritParams COJO_args
#' @returns Executable path.
#' @keywords internal
#' @importFrom echoconda find_executables_remote
COJO_find_executable <- function(gcta_path=NULL,
                                 verbose=TRUE){
    echoconda::find_executables_remote(path = gcta_path,
                                       tool = "gcta",
                                       verbose = verbose)[[1]]
}