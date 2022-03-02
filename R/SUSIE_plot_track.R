SUSIE_plot_track <- function(plot_track_fit,
                             fitted_bhat,
                             max_causal,
                             file_prefix){
    if(plot_track_fit){ 
        dir.create(dirname(file_prefix), showWarnings = FALSE, recursive = TRUE)
        try({susieR::susie_plot_iteration(model = fitted_bhat, 
                                          L = max_causal, 
                                          file_prefix = file_prefix)})
    }
}