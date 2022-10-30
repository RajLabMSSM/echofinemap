POLYFUN_example_data <- function(type=c("sumstats",
                                        "weights",
                                        "annotations")){
    type <- tolower(type)[[1]]
    ex_dir <- system.file("tools",
                          "polyfun",
                          "example_data",
                          package="echofinemap") 
    if(type=="sumstats"){
        ex_parq <- file.path(ex_dir,"sumstats.parquet") 
        tmp_parq <- tempfile(fileext = basename(ex_parq))
        ot <- file.copy(from = ex_parq, 
                        to = tmp_parq, 
                        overwrite = TRUE)
        return(tmp_parq)
    } else if(type=="annotations"){
        annotations_path <- file.path(ex_dir,"annotations.")
        return(annotations_path)
    } else if(type=="weights"){
        weights_path <- file.path(ex_dir,"weights.")
        return(weights_path)
    } else if(is.null(type)){
        all_files <- list.files(path = ex_dir,
                                full.names = TRUE,
                                recursive = TRUE)
        return(all_files)
    }
}
