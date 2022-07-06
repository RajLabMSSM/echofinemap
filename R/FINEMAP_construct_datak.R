FINEMAP_construct_datak <- function(prior_k, 
                                    locus_dir,
                                    n_causal,
                                    verbose=TRUE){
    if(is.null(prior_k)) return(NULL)
    data.k <- data.table::as.data.table(
        lapply(seq_len(n_causal), function(x){prior_k})
    )
    data.k_path <- file.path(locus_dir,"FINEMAP","data.k")
    data.table::fwrite(x = data.k, 
                       file = data.k_path,
                       col.names = FALSE,
                       row.names = FALSE,
                       sep = " ")
    return(data.k_path)
}