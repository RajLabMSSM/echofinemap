PAINTOR_input_file <- function(locus_dir,
                               PT_results_path,
                               verbose = TRUE){
    messager("+ PAINTOR:: Preparing input.files.", v=verbose)
    path <- file.path(PT_results_path, "input.files")
    data.table::fwrite(x = list(basename(locus_dir)),
                       file = path,
                       sep="\n") 
    return(path)
}