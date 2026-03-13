#' Prepare PAINTOR annotations
#'
#' Convert BED annotation files into a PAINTOR-format annotation matrix.
#' For each SNP in the locus file, checks whether its position falls within
#' any interval in each BED file, producing a binary (0/1) matrix.
#'
#' This is a pure-R reimplementation of the original
#' \code{AnnotateLocus.py} Python 2.7 utility bundled with PAINTOR.
#'
#' @param BED_paths Character vector of paths to BED files
#' (can be gzipped). Each BED file becomes one annotation column.
#' @param dat_merged data.table with at least \code{CHR} and \code{POS} columns
#' containing the SNPs to annotate.
#' @param PT_results_path Directory where the annotation matrix will be written.
#' @param locus_dir Locus directory (used for naming the output file).
#' @param verbose Print messages.
#' @returns Path to the annotation matrix file.
#' @keywords internal
#' @importFrom data.table fread fwrite data.table
PAINTOR_prepare_annotations <- function(BED_paths,
                                        dat_merged,
                                        PT_results_path,
                                        locus_dir,
                                        verbose = TRUE){

    messager("++ PAINTOR:: Creating annotation matrix from",
             length(BED_paths),"BED files.", v = verbose)

    chrom <- unique(dat_merged$CHR)
    positions <- dat_merged$POS

    annot_matrix <- data.table::data.table(
        matrix(0L, nrow = length(positions), ncol = 0)
    )

    for(bed_path in BED_paths){
        annot_name <- gsub("\\.bed(\\.gz)?$", "", basename(bed_path))
        messager("+++ PAINTOR:: Processing annotation:", annot_name,
                 v = verbose)
        bed <- tryCatch({
            data.table::fread(bed_path, header = FALSE,
                              select = 1:3,
                              col.names = c("chr","start","end"))
        }, error = function(e){
            messager("Warning: Could not read", bed_path, ":",
                     e$message, v = verbose)
            return(NULL)
        })
        if(is.null(bed) || nrow(bed) == 0){
            annot_matrix[[annot_name]] <- rep(0L, length(positions))
            next
        }
        ## Normalize chromosome format (handle chr1 vs 1)
        bed$chr <- gsub("^chr", "", bed$chr)
        chrom_clean <- gsub("^chr", "", as.character(chrom))
        ## Filter to matching chromosome
        bed_chr <- bed[bed$chr == chrom_clean, ]
        ## Check each SNP position against BED intervals
        overlaps <- vapply(positions, function(pos){
            any(bed_chr$start <= pos & pos <= bed_chr$end)
        }, logical(1))
        annot_matrix[[annot_name]] <- as.integer(overlaps)
    }

    ## Write annotation matrix in PAINTOR format (space-delimited)
    annotations_file <- file.path(
        PT_results_path,
        paste0(basename(locus_dir), ".annotations.txt")
    )
    data.table::fwrite(annot_matrix,
                       file = annotations_file,
                       sep = " ",
                       quote = FALSE)
    messager("+++ PAINTOR:: Annotation-SNP overlap summaries:", v = verbose)
    if(verbose && ncol(annot_matrix) > 0){
        messager(paste(capture.output(colSums(annot_matrix)), collapse = "\n"),
                 v = verbose)
    }
    return(annotations_file)
}
