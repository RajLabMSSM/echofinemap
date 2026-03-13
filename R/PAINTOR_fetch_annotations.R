#' Fetch PAINTOR functional annotations
#'
#' Download the comprehensive PAINTOR functional annotation library
#' (Functional_Annotations.tar.gz, ~7GB) from BOX and cache it locally.
#' The library contains 10,000+ annotations across functional genomics
#' datasets (FANTOM5 enhancers, RoadMap ChromHMM, TFBS, DHS, etc.)
#' in hg19 coordinates.
#'
#' @param cache_dir Directory to cache the downloaded annotations.
#'   Defaults to \code{tools::R_user_dir("echofinemap", "cache")}.
#' @param force_new Force re-download even if cached.
#' @param verbose Print messages.
#' @returns Path to the extracted annotations directory.
#' @keywords internal
PAINTOR_fetch_annotations <- function(
        cache_dir = tools::R_user_dir("echofinemap", "cache"),
        force_new = FALSE,
        verbose = TRUE){

    annot_dir <- file.path(cache_dir, "Functional_Annotations")
    marker <- file.path(annot_dir, ".download_complete")
    if(dir.exists(annot_dir) && file.exists(marker) && !force_new){
        messager("PAINTOR:: Using cached annotations:", annot_dir,
                 v = verbose)
        return(annot_dir)
    }
    ## BOX shared link for Functional_Annotations.tar.gz
    ## Hosted by PAINTOR authors (Kichaev et al.)
    box_url <- paste0(
        "https://ucla.app.box.com/index.php?",
        "rm=box_download_shared_file&",
        "shared_name=x47apvgv51au1rlmuat8m4zdjhcniv2d&",
        "file_id=f_102443634620"
    )
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    tarball <- file.path(cache_dir, "Functional_Annotations.tar.gz")

    messager("PAINTOR:: Downloading functional annotations (~7GB)...",
             v = verbose)
    messager("PAINTOR:: This is a one-time download; ",
             "annotations will be cached at:", cache_dir, v = verbose)
    exit_code <- utils::download.file(
        url = box_url,
        destfile = tarball,
        mode = "wb",
        quiet = !verbose
    )
    if(exit_code != 0 || !file.exists(tarball)){
        stop("Failed to download PAINTOR annotations from BOX. ",
             "You can manually download from:\n",
             "  https://ucla.box.com/s/x47apvgv51au1rlmuat8m4zdjhcniv2d")
    }
    ## Verify SHA1 if tools::md5sum is available (base R)
    expected_sha1 <- "a66a0cd136e6b57e75028c36e3d975ba2d5ac54f"
    actual_sha1 <- system2("shasum", args = c("-a", "1", tarball),
                           stdout = TRUE, stderr = TRUE)
    if(length(actual_sha1) == 1 && !inherits(actual_sha1, "error")){
        actual_sha1 <- sub("\\s.*", "", actual_sha1)
        if(actual_sha1 != expected_sha1){
            warning("SHA1 mismatch for PAINTOR annotations tarball. ",
                    "Expected: ", expected_sha1, " Got: ", actual_sha1)
        }
    }
    messager("PAINTOR:: Extracting annotations...", v = verbose)
    utils::untar(tarball, exdir = cache_dir)
    if(!dir.exists(annot_dir)){
        stop("Extraction failed: expected directory not found at ", annot_dir)
    }
    ## Write marker file
    writeLines(as.character(Sys.time()), marker)
    ## Clean up tarball to save space
    unlink(tarball)
    messager("PAINTOR:: Annotations cached at:", annot_dir, v = verbose)
    return(annot_dir)
}


#' List PAINTOR annotation BED files
#'
#' List all BED annotation files from the PAINTOR functional annotation
#' library, optionally filtered by category keyword.
#'
#' @param annot_dir Path to the extracted annotations directory
#'   (from \code{PAINTOR_fetch_annotations}).
#' @param categories Character vector of category keywords to filter by
#'   (e.g., \code{"FANTOM5"}, \code{"ChromHMM"}, \code{"DHS"}, \code{"TFBS"}).
#'   If \code{NULL}, returns all BED files.
#' @param verbose Print messages.
#' @returns Character vector of BED file paths.
#' @keywords internal
PAINTOR_list_annotations <- function(annot_dir,
                                     categories = NULL,
                                     verbose = TRUE){
    bed_files <- list.files(annot_dir,
                            pattern = "\\.bed(\\.gz)?$",
                            full.names = TRUE,
                            recursive = TRUE)
    messager("PAINTOR:: Found", length(bed_files),
             "annotation BED files.", v = verbose)
    if(!is.null(categories) && length(categories) > 0){
        pattern <- paste(categories, collapse = "|")
        bed_files <- bed_files[grepl(pattern, bed_files, ignore.case = TRUE)]
        messager("PAINTOR:: Filtered to", length(bed_files),
                 "BED files matching:", paste(categories, collapse = ", "),
                 v = verbose)
    }
    return(bed_files)
}
