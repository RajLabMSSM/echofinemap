#' Determine data types (GWAS, QTL)
#'
#' @keywords internal
PAINTOR_datatype_handler <- function(locus_dir,
                                     dat_prefixes = NULL,
                                     annot_prefixes = NULL,
                                     verbose = TRUE){
    #### Set up paths ####
    gwas_dir <- file.path(locus_dir,"GWAS")
    annot_dir <- file.path(locus_dir,"annotations")
    locus <- basename(locus_dir)
    #### Check args ####
    if(!all(is.null(dat_prefixes)) &&
       !all(is.null(annot_prefixes))){
        messager("++ PAINTOR::",
                 length(dat_prefixes),"primary data and",
                 length(annot_prefixes),"annotations detected.",
                 "Feeding both types into PAINTOR.", v=verbose)
        subset_path <- file.path(
            annot_dir,
            paste0(c(dat_prefixes,annot_prefixes),collapse="--"),
            locus)
        
    } else if(!all(is.null(dat_prefixes)) &&
              all(is.null(annot_prefixes))){
        messager("++ PAINTOR:: Only primary data detected.",
                 "Feeding",length(dat_prefixes),
                 "GWAS dataset(s) into PAINTOR.", v=verbose)
        subset_path <- file.path(gwas_dir,dat_prefixes, locus)
        
    } else if(all(is.null(dat_prefixes)) &&
              !all(is.null(annot_prefixes))){
        messager("++ PAINTOR:: Only annotations detected.",
                 "Feeding",length(annot_prefixes),
                 "annotation dataset(s) into PAINTOR.", v=verbose)
        subset_path <- file.path(annot_dir,
                                 paste0(annot_prefixes,collapse="--"), locus)
        
    } else {
        stp <- paste(
            "++ PAINTOR:: Neither primary data nor annotations detected.",
            "Please enter at least one valid dataset."
        )
        stop(stp)
    }
    PT_results_path <- file.path(subset_path,"PAINTOR")
    if(length(PT_results_path)>1){
        PT_results_path <- PT_results_path[1]
        messager("Warning: >1 primary dataset detected.",
                 "Will only write results to the",
                 "directory of the first dataset:",
                 basename(dirname(dirname(PT_results_path)))
                 )
    }
    dir.create(PT_results_path, showWarnings = FALSE, recursive = TRUE)
    messager("++ PAINTOR:: Results will be stored in  ==> ", PT_results_path,
             v=verbose)
    return(PT_results_path)
}
