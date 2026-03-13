#' Download annotations for PAINTOR
#'
#' Download annotations to perform functional fine-mapping with PAINTOR.
#' Supports three annotation sources:
#' \describe{
#'   \item{annot_paintor}{The comprehensive PAINTOR functional annotation
#'     library (~7GB, 10,000+ annotations). Downloaded once and cached.}
#'   \item{annot_xgr}{Annotations from XGR via \link[echoannot]{XGR_query}.}
#'   \item{annot_roadmap}{Annotations from Roadmap Epigenomics via
#'     \link[echoannot]{ROADMAP_query}.}
#' }
#' @param annot_paintor Character vector of PAINTOR annotation category
#'   keywords to include (e.g., \code{"FANTOM5"}, \code{"ChromHMM"},
#'   \code{"DHS"}, \code{"TFBS"}). Set to \code{"all"} to use all annotations,
#'   or \code{NULL} to skip (default).
#' @inheritParams multifinemap
#' @inheritParams PAINTOR
#' @keywords internal
#' @importFrom echoannot XGR_query ROADMAP_query
PAINTOR_download_annotations <- function(dat_merged,
                                         locus_dir,
                                         PT_results_path,
                                         annot_paintor = NULL,
                                         annot_sample = FALSE,
                                         annot_xgr = NULL,
                                         annot_roadmap = NULL,
                                         chrom_states = NULL,
                                         use_annotations = TRUE,
                                         conda_env = "echoR_mini",
                                         nThread = 1,
                                         verbose = TRUE){
    #### Create annot file ####
    messager("+ PAINTOR:: Preparing Annotation Matrix File.", v = verbose)
    .annotations_file <- file.path(PT_results_path,
                                   paste0(basename(locus_dir),
                                          ".annotations.txt"))
    paths <- list()
    #### Use default priors of 1 #####
    if(isFALSE(use_annotations)){
        messager("++ PAINTOR:: no_annotations=TRUE.",
                 "Prior Probability set to 1 for all SNPs.")
        data.table::fwrite(x = data.frame(rep(1, nrow(dat_merged))),
                           file = .annotations_file,
                           sep = " ",
                           quote = FALSE)
        return(.annotations_file)

    #### Use real priors ####
    } else {
        #### PAINTOR: full annotations ####
        if(!is.null(annot_paintor)){
            messager("++ Preparing annotations via PAINTOR library.",
                     v = verbose)
            annot_dir <- PAINTOR_fetch_annotations(verbose = verbose)
            categories <- if(identical(annot_paintor, "all")) NULL
                          else annot_paintor
            paths[["PAINTOR"]] <- PAINTOR_list_annotations(
                annot_dir = annot_dir,
                categories = categories,
                verbose = verbose)
        }
        #### XGR ####
        if(!is.null(annot_xgr)){
            messager("++ Preparing annotations via XGR.", v = verbose)
            xgr.gr <- echoannot::XGR_query(
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
            messager("++ Preparing annotations via ROADMAP API.", v = verbose)
            if(annot_roadmap == "all"){annot_roadmap <- ""}
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
