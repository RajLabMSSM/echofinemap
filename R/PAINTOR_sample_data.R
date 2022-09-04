#' Find PAINTOR sample data
#'
#' @keywords internal
#' @importFrom data.table rbindlist
#' @importFrom stringr str_split
PAINTOR_sample_data <- function(
        annot_dir = system.file("tools","PAINTOR_V3.0","SampleData",
                                package = "echofinemap"),
        pattern = "\\.annotations",
        recursive = TRUE,
        read = FALSE,
        verbose = TRUE){

    files <- list.files(annot_dir, pattern, 
                        full.names = TRUE, 
                        recursive = recursive)
    messager("PAINTOR:: Using",formatC(length(files),big.mark = ","),
             "sampleData annotations.",v=verbose)
    if(isTRUE(read)){
        annot <- lapply(files, function(x){
            data.table::fread(x)
        }) |> `names<-`(gsub("Locus","",
                         stringr::str_split(basename(files),"\\.",
                                            simplify = TRUE)[,1])) |>
            data.table::rbindlist(use.names = TRUE, idcol = "Locus") 
        return(annot)
    } else {
        return(files)
    }
}
