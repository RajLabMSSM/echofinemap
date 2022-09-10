#' Download annotations for PAINTOR
#'
#' @keywords internal
PAINTOR_download_annotations <- function(dat_merged,  
                                         locus_dir,
                                         PT_results_path,
                                         annot_sample = FALSE,
                                         annot_xgr = NULL,
                                         annot_roadmap = NULL,
                                         chrom_states = NULL,
                                         use_annotations = TRUE,
                                         conda_env = "echoR_mini",
                                         nThread = 1,
                                         verbose = TRUE){
    ## removed annot_paintor= arg until I figure out 
    ## how get programmatically get PAINTOR annotations.
    annot_paintor = FALSE; 
    #### Create annot file ####
    messager("+ PAINTOR:: Preparing Annotation Matrix File.",v=verbose)
    .annotations_file <- file.path(PT_results_path, 
                                   paste0(basename(locus_dir),
                                          ".annotations.txt"))
    paths <- list()
    #### Use default priors of 1 #####
    if(isFALSE(use_annotations)){
        messager("++ PAINTOR:: no_annotations=TRUE.",
                 "Prior Probability set to 1 for all SNPs.") 
        data.table::fwrite(x = data.frame(rep(1,nrow(dat_merged))),
                           file = .annotations_file,
                           sep = " ", 
                           quote = FALSE)
        return(.annotations_file)
        
    #### Use real priors ####
    } else{   
        #### PAINTOR: full annotations ####
        if(!is.null(annot_paintor)){
            # paths[["XGR"]] <- -echoannot::PAINTOR_query()
        }
        #### XGR ####
        if(!is.null(annot_xgr)){
            messager("++ Preparing annotations via XGR.",v=verbose) 
            xgr.gr <- echoannot::XGR_download_and_standardize(
                lib.selections = annot_xgr, 
                as_grangesList = TRUE, 
                dat = dat_merged,
                nThread = nThread)
            paths[["XGR"]] <- echodata::granges_to_bed(
                grlist = xgr.gr,
                save_dir = PT_results_path, 
                gzip = TRUE) 
        }
        #### ROADMAP ####
        if(!is.null(annot_roadmap)){
            messager("++ Preparing annotations via ROADMAP API.",v=verbose) 
            # Gather roadmap annotations
            if(annot_roadmap=="all"){annot_roadmap <- ""} 
            paths[["ROADMAP"]] <- echoannot::ROADMAP_query(
                query_dat = dat_merged,
                keyword_query = annot_roadmap,
                chrom_states = chrom_states,
                conda_env = conda_env,
                return_paths = TRUE,
                nThread = nThread, 
                verbose = verbose)
        }
    }
    return(paths)
}
