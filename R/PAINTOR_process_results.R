#' Process PAINTOR results
#'
#'  @keywords internal
PAINTOR_process_results <- function(PT_results_path,
                                    locus_name="Locus1"){
    PAINTOR_results <- data.table::fread(file.path(PT_results_path, paste0(locus_name,".results.txt")),
                                         nThread = 1) |>
        arrange(desc(Posterior_Prob))
    return(data.table::data.table(PAINTOR_results))
}

