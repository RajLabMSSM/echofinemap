#' Report the number of annotations > 0
#'
#'  @keywords internal
PAINTOR_survey_annotation <- function(PT_results_path,
                                      locus_name="Locus1"){
    anno <- data.table::fread(file.path(PT_results_path, 
                                        paste0(locus_name,".annotations.txt")))
    column_sums <- sort(colSums(anno), decreasing = TRUE)
    signals <- column_sums[column_sums > 0]
    print(signals)
}
