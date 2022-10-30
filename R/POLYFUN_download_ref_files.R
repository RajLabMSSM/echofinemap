#' Download reference files
#' 
#' Download 1000 Genomes reference files.
#' @family polyfun
#' @keywords internal
#' @importFrom downloadR downloader
#' @importFrom utils untar
#' @returns File prefix.
#' 
#' @source
#' \code{
#' ref_prefix <- POLYFUN_download_ref_files()
#' }
POLYFUN_download_ref_files <- function(
        alkes_url=paste("https://data.broadinstitute.org/alkesgroup",
                        "LDSCORE/1000G_Phase1_plinkfiles.tgz",sep="/"),
    output_dir= tools::R_user_dir(package = "echofinemap",
                                  which = "cache"),
    force_overwrite=FALSE,
    return_prefix=TRUE,
    download_method="axel",
    conda_env="echoR_mini",
    verbose=TRUE){
    
    # echoverseTemplate:::source_all()
    # echoverseTemplate:::args2vars(POLYFUN_download_ref_files)
    # 
    out_gz <- file.path(output_dir,basename(alkes_url)) 
    out <- gsub("\\.tgz","",out_gz) 
    #### Download if it doesn't exist ####
    if(!file.exists(out)){
        messager("+ POLYFUN:: Downloading reference files.",v=verbose)
        out_gz <- downloadR::downloader(input_url = alkes_url,
                                        output_dir = output_dir,  
                                        force_overwrite = force_overwrite,
                                        download_method = download_method, 
                                        conda_env = conda_env)
        messager("+ POLYFUN:: Unzipping reference files.",v=verbose) 
        utils::untar(tarfile = unname(out_gz), 
                     exdir = out)
    }   
    #### List reference files ####
    files <- list.files(path = out, 
                        pattern = "*.bim", 
                        full.names = TRUE,
                        recursive = TRUE)
    #### Return ####
    if(isTRUE(return_prefix)){
        ref_prefix <- gsub(".?.bim","", files[[1]])
        return(ref_prefix)
    } else {
        return(files)
    } 
}
