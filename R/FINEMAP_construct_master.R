#' Construct the \code{FINAMAP} master file
#'
#' Creates and saves the master file
#' which tells \code{FINEMAP} where to find each input file.
#' @family FINEMAP
#' @keywords internal
#' @source \url{http://www.christianbenner.com}
#' @source 
#' \code{
#' locus_dir <- file.path(tempdir(),echodata::locus_dir)
#' master_path <- echofinemap:::FINEMAP_construct_master(locus_dir=locus_dir,
#'                                                       n_samples=25000)
#' }
FINEMAP_construct_master <- function(locus_dir,
                                     n_samples,
                                     dir="FINEMAP",
                                     dataset_number=1,
                                     data.k_path=NULL,
                                     verbose=TRUE){
    messager("Constructing master file.",v=verbose)
    # For full list of parameters: http://www.christianbenner.com
    header <- "z;ld;snp;config;cred;log;n_samples"
    files <- c("data.z",  # [required input]
               "data.ld", # [required input]
               "data.snp", # [output]
               "data.config", # [optional output]
               "data.cred", # [optional output]
               "data.log"# [optional output]
    )
    paths_list <- paste(c(file.path(dir,files),n_samples),
                        collapse = ";")
    #### Add priors file (if used) ####
    if(!is.null(data.k_path)){ 
        paths_list <- paste(paths_list, 
                            file.path(dir,basename(data.k_path)),sep=";")
        header <- paste(header,"k",sep = ";")
    }
    # Write master file
    dir.create(file.path(locus_dir, "FINEMAP"), 
               recursive = TRUE, showWarnings  = FALSE)
    master_path <- file.path(locus_dir,"FINEMAP","master")
    data.table::fwrite(x = list(header,paths_list), 
                       file = master_path,
                       quote=FALSE, sep="\n")
    return(master_path)
}
