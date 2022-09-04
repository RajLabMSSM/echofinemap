#' Prepare PAINTOR annotations
#'
#' @keywords internal
PAINTOR_prepare_annotations <- function(paintor_path=NULL,
                                        BED_paths,
                                        PT_results_path,
                                        locus_name,
                                        remove_BED=FALSE){
    paintor_path <- PAINTOR_find_folder(paintor_path=paintor_path)
    .locus_file <- file.path(PT_results_path, locus_name)
    .annotations_file <- file.path(PT_results_path, paste0(locus_name,".annotations.txt"))
    
    messager("++ PAINTOR:: Merging and formatting BED files using PAINTOR utility.")
    ## Use PAINTOR utility to format merge BED files
    messager("+++ PAINTOR:: Decompressing BED files.")
    gz.files <- grep("*.gz",BED_paths,value = TRUE)
    if(length(gz.files)>0){
        for (gz in gz.files){
            # Unzip but keep original files
            try({gunzip(gz, overwrite=TRUE, remove=FALSE)})
        }
    }
    
    BED_paths <- gsub("*.gz","", BED_paths)
    annotation_paths <- file.path(PT_results_path,"annotation_paths.txt")
    # Wait until the BED files are decompressed before trying to write them to a file
    while(any(!file.exists(BED_paths))){
        Sys.sleep(.001)
    }
    messager("+++ PAINTOR:: Writing annotations paths file to  ==> ",annotation_paths)
    data.table::fwrite(list(BED_paths),
                       annotation_paths,
                       sep="\n")
    
    
    cmd <-  paste("python2.7",file.path(paintor_path,"PAINTOR_Utilities","AnnotateLocus.py"),
                  "--input", file.path(PT_results_path,"annotation_paths.txt"),
                  "--locus", .locus_file,
                  "--out", .annotations_file,
                  "--chr","CHR",
                  "--pos","POS")
    chimera=F
    if(chimera){cmd <- paste("ml python/2.7.10 &&",cmd)}
    system(cmd)
    messager("+++ PAINTOR:: Annotation--SNP overlap summaries:")
    colSums( data.table::fread(.annotations_file))
    if(remove_BED){
        messager("+++ PAINTOR:: Removing temporary decompressed BED files.")
        file.remove(BED_paths)
    }
}
