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
                                     dataset_number=1,
                                     file.k=NA,
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
    if(!is.na(file.k)){ files <- append(files, file.k) }
    paths_list <- paste(c(file.path("FINEMAP",files),n_samples),
                        collapse = ";")
    # Write master file
    dir.create(file.path(locus_dir, "FINEMAP"), 
               recursive = TRUE, showWarnings  = FALSE)
    master_path <- file.path(locus_dir,"FINEMAP","master")
    data.table::fwrite(list(header,paths_list), master_path,
                       quote=FALSE, sep="\n")
    return(master_path)
}
