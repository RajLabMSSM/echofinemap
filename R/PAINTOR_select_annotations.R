PAINTOR_select_annotations <- function(annot_dir,
                                       types = c("Roadmap_ChromeHMM_15state",
                                                  "RoadMap_Enhancers",
                                                  "RoadMap_Promoter",
                                                  "TFBS"),
                                       verbose = TRUE){
    if(is.null(types)){ 
        BED_paths <- list.files(annot_dir, full.names = TRUE, recursive = TRUE)
    } else { 
        BED_paths <- list.files(file.path(annot_dir, types), 
                                full.names = TRUE)
    }
    messager("PAINTOR::",length(BED_paths),"matching annotations found.",
             v=verbose)
    return(BED_paths)
}

