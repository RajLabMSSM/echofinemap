#' Download 1000 Genomes reference files
#' @keywords internal
#' @family polyfun
#' @examples
#' \dontrun{
#' ref.prefix <- POLYFUN_download_ref_files(force_overwrite=TRUE)
#' }
POLYFUN_download_ref_files <- function(alkes_url="https://data.broadinstitute.org/alkesgroup/LDSCORE/1000G_Phase1_plinkfiles.tgz",
                                       # output_path="/sc/arion/projects/pd-omics/data/1000_Genomes/Phase1",
                                       results_dir="./results",
                                       force_overwrite=FALSE,
                                       download_method="wget",
                                       conda_env="echoR"
){
    output_path <- file.path(results_dir,"resources/1000Genomes_Phase1")
    file_name <- basename(alkes_url)
    dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
    # cmd <- paste("wget",
    #              alkes_url,
    #              "--no-parent", # Makes everything waayyyyyy faster
    #              "& mv",file_name, output_path,
    #              "& tar zxvf",output_path,"--strip 1")
    # paste(cmd)
    # system(cmd)
    messager("+ POLYFUN:: Downloading reference files...")
    out_file <- downloader(input_url = alkes_url,
                           output_path = output_path,
                           force_overwrite = force_overwrite,
                           download_method = download_method,
                           background = FALSE,
                           conda_env=conda_env)
    messager("+ POLYFUN:: Unzipping reference files...")
    system(paste("tar zxvf",file.path(output_path, file_name),
                 "--strip 1",
                 ifelse(force_overwrite,"","-k"),
                 "-C",output_path))
    # List reference files
    ref.prefix <- list.files(output_path, pattern = "*.bim", full.names = TRUE)[1]
    ref.prefix <- gsub(".?.bim","", ref.prefix)
    return(ref.prefix)
}

