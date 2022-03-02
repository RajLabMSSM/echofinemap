#' Read parquet file as data.frame
#'
#' @param method Specify whether to use \code{\link{SparkR}} (R)
#' or \code{pandas} (Python + \code{reticulate}).
#' @export
#' @family polyfun
#' @examples
#' \dontrun{
#' parquet_path <- system.file("tools/polyfun/example_data/weights.10.l2.ldscore.parquet", package = "echofinemap")
#' #### Using python (pandas) - default ####
#' parq <- read_parquet(parquet_path=parquet_path, method="pandas")
#' 
#' #### Using R (SparkR) ####
#' parq <- read_parquet(parquet_path=parquet_path, method="sparkR")
#' }
read_parquet <- function(parquet_path,
                         conda_env="echoR",
                         # conda = "/opt/anaconda3/bin/conda",
                         method="pandas",
                         verbose=TRUE){
    if(method=="SparkR"){
        requireNamespace("SparkR")
        messager("+ Importing parquet file with `SparkR (R)`",v=verbose)
        SparkR::sparkR.session()
        parquor <- SparkR::read.parquet(parquet_path)
        parquor <- SparkR::as.data.frame(parquor) 
    } else {
        messager("+ Importing parquet file with `pandas (Python)`",v=verbose) 
        #### Create echoR conda env if you haven't already ####
        if(!echoconda::env_exists(conda_env = conda_env)){
            echoconda::env_from_yaml(verbose = verbose) # Installs echoR by default
            conda_env <- "echoR"
        }
        echoconda::activate_env(conda_env = conda_env)
        # python <- echoconda::find_python_path(conda_env = conda_env)
        # Sys.setenv(RETICULATE_PYTHON=python)
        # reticulate::use_python(python = python) 
        pd <- reticulate::import("pandas")
        parquor <- pd$read_parquet(parquet_path)
    }
    return(data.table::data.table(parquor))
}
